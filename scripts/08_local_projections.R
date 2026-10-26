rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, 
               sandwich, 
               lmtest)

#sample configuration

sample_name <- Sys.getenv("SAMPLE", "official")

config <- switch(
  sample_name,
  official = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = c("shock_2016", "shock_covid"),
    suffix = "_official"
  ),
  official_nsa = list(
    file = "Data/csv/timor_quarterly_official_nsa_log_and_dummys.csv",
    exo = c("shock_2016", "shock_covid", "q2", "q3", "q4"),
    suffix = "_official_nsa"
  ),
  long = list(
    file = "Data/csv/timor_quarterly_log_and_dummys.csv",
    exo = c(
      "shock_2006", "shock_2009", "shock_2012",
      "shock_2016", "shock_covid"
    ),
    suffix = "_long"
  ),
  stop("Unknown SAMPLE. Use official, official_nsa, or long.")
)

data <- readr::read_csv(config$file, show_col_types = FALSE)
exo <- data %>%
  dplyr::select(dplyr::all_of(config$exo)) %>%
  as.matrix()

H <- 12
LAGS <- 2

core_vars <- c("ln_gov_exp", "ln_gdp_non", "ln_imports")
response_vars <- c(
  "ln_gov_exp", "ln_imports", "ln_gdp_non", "ln_cpi", "ln_credit"
)
response_labels <- c(
  "ln_gov_exp" = "Government expenditure",
  "ln_imports" = "Imports",
  "ln_gdp_non" = "Non-oil GDP",
  "ln_cpi" = "CPI",
  "ln_credit" = "Private credit"
)

#reduced-form VAR and recursively identified fiscal shock

Y <- data %>%
  dplyr::select(dplyr::all_of(core_vars)) %>%
  as.matrix()

T <- nrow(Y)
t0 <- LAGS + 1
tt <- t0:T
n_used <- length(tt)

#lagged regressors: constant + LAGS lags of the core variables + exogenous.
X <- matrix(1, nrow = n_used, ncol = 1)
for (l in seq_len(LAGS)) {
  X <- cbind(X, Y[(tt - l), , drop = FALSE])
}
X <- cbind(X, exo[tt, , drop = FALSE])
colnames(X) <- c(
  "const",
  paste0(rep(core_vars, LAGS), "_lag", rep(seq_len(LAGS), each = length(core_vars))),
  colnames(exo)
)

B <- solve(crossprod(X), crossprod(X, Y[tt, , drop = FALSE]))
U <- Y[tt, , drop = FALSE] - X %*% B

Sigma <- crossprod(U) / (nrow(U) - ncol(X))
A <- t(chol(Sigma))
eps <- U %*% solve(A)

shock_raw <- as.numeric(eps[, 1])
sd_shock <- stats::sd(shock_raw)
shock <- shock_raw / sd_shock

#local projections

lp_one <- function(response, h) {
  idx <- seq_len(length(tt) - h)
  y <- data[[response]][tt[idx] + h]

  controls <- cbind(
    shock = shock[idx],
    X[idx, , drop = FALSE]
  )

  fit <- stats::lm(y ~ controls - 1)
  vcov_hac <- sandwich::NeweyWest(
    fit,
    lag = min(h + 1, 6),
    prewhite = FALSE,
    adjust = TRUE
  )

  coefs <- stats::coef(fit)
  se <- sqrt(diag(vcov_hac))

  tibble::tibble(
    response = response,
    horizon = h,
    irf = unname(coefs["controlsshock"]),
    se = unname(se["controlsshock"]),
    n_obs = length(y)
  )
}

lp_results <- purrr::map_dfr(response_vars, function(v) {
  purrr::map_dfr(0:H, function(h) lp_one(v, h))
})

#normalise so that the impact response of government expenditure is 1%.
impact_gov <- lp_results %>%
  dplyr::filter(response == "ln_gov_exp", horizon == 0) %>%
  dplyr::pull(irf)

if (length(impact_gov) != 1 || impact_gov == 0) {
  stop("Could not recover a valid impact response for government expenditure.")
}

lp_results <- lp_results %>%
  dplyr::mutate(
    scale = 0.01 / impact_gov,
    irf_1pct = irf * scale,
    se_1pct = se * scale,
    lower68 = irf_1pct - se_1pct,
    upper68 = irf_1pct + se_1pct,
    lower90 = irf_1pct - 1.645 * se_1pct,
    upper90 = irf_1pct + 1.645 * se_1pct,
    response_label = dplyr::recode(response, !!!response_labels)
  )

readr::write_csv(
  lp_results,
  paste0("Data/csv/lp_irf", config$suffix, ".csv")
)

#plot

plot_data <- lp_results %>%
  dplyr::filter(response != "ln_gov_exp") %>%
  dplyr::mutate(
    response_label = factor(
      response_label,
      levels = c("Imports", "Non-oil GDP", "CPI", "Private credit")
    )
  )

lp_plot <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = horizon, y = irf_1pct)
) +
  ggplot2::geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower90, ymax = upper90),
    fill = "gray80", alpha = 0.55
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower68, ymax = upper68),
    fill = "gray55", alpha = 0.55
  ) +
  ggplot2::geom_line(color = "#004c99", linewidth = 1) +
  ggplot2::facet_wrap(~ response_label, scales = "free_y") +
  ggplot2::labs(
    x = "Quarters after the shock",
    y = "Response to a 1% increase in government expenditure (log points)",
    caption = "Bands: 68% and 90% HAC confidence intervals."
  ) +
  ggplot2::theme_classic(base_family = "serif", base_size = 12) +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  paste0("graphics/lp_irf", config$suffix, ".pdf"),
  plot = lp_plot,
  width = 10, height = 7
)

#diagnostics

readr::write_csv(
  tibble::tibble(
    sample = sample_name,
    n_obs = T,
    n_used = n_used,
    lags = LAGS,
    horizon = H,
    shock_sd = sd_shock,
    impact_gov = impact_gov
  ),
  paste0("Data/csv/lp_diagnostics", config$suffix, ".csv")
)

message("Local projections finished for sample: ", sample_name)
print(
  lp_results %>%
    dplyr::filter(response != "ln_gov_exp") %>%
    dplyr::select(response_label, horizon, irf_1pct, lower68, upper68) %>%
    dplyr::slice_head(n = 8)
)
