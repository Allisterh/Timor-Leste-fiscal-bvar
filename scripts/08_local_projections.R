rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, lpirfs)

#sample configuration

sample_name <- Sys.getenv("SAMPLE", "official")

config <- switch(
  sample_name,
  official = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = c("shock_2016", "shock_covid"),
    suffix = "_official",
    core_vars = c("ln_gov_exp", "ln_imports", "ln_gdp_non"),
    fiscal_var = "ln_gov_exp",
    response_vars = c(
      "ln_gov_exp", "ln_imports", "ln_gdp_non",
      "ln_private_demand", "ln_gdp_ex_gov", "ln_cpi", "ln_credit"
    ),
    response_labels = c(
      "ln_gov_exp" = "Government expenditure",
      "ln_imports" = "Imports",
      "ln_gdp_non" = "Non-oil GDP",
      "ln_private_demand" = "Private demand",
      "ln_gdp_ex_gov" = "GDP excl. GFC",
      "ln_cpi" = "CPI",
      "ln_credit" = "Private credit"
    ),
    response_levels = c(
      "Imports", "Non-oil GDP", "Private demand", "GDP excl. GFC",
      "CPI", "Private credit"
    )
  ),
  official_broad = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = c("shock_2016", "shock_covid"),
    suffix = "_official_broad",
    core_vars = c("ln_gov_broad", "ln_imports", "ln_gdp_non"),
    fiscal_var = "ln_gov_broad",
    response_vars = c(
      "ln_gov_broad", "ln_imports", "ln_gdp_non",
      "ln_private_demand", "ln_cpi", "ln_credit"
    ),
    response_labels = c(
      "ln_gov_broad" = "Broad government expenditure",
      "ln_imports" = "Imports",
      "ln_gdp_non" = "Non-oil GDP",
      "ln_private_demand" = "Private demand",
      "ln_cpi" = "CPI",
      "ln_credit" = "Private credit"
    ),
    response_levels = c(
      "Imports", "Non-oil GDP", "Private demand", "CPI", "Private credit"
    )
  ),
  official_broad_dp = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = c("shock_2016", "shock_covid"),
    suffix = "_official_broad_dp",
    core_vars = c("ln_gov_broad_dp", "ln_imports", "ln_gdp_non"),
    fiscal_var = "ln_gov_broad_dp",
    response_vars = c(
      "ln_gov_broad_dp", "ln_imports", "ln_gdp_non",
      "ln_private_demand", "ln_cpi", "ln_credit"
    ),
    response_labels = c(
      "ln_gov_broad_dp" = "Public and donor expenditure",
      "ln_imports" = "Imports",
      "ln_gdp_non" = "Non-oil GDP",
      "ln_private_demand" = "Private demand",
      "ln_cpi" = "CPI",
      "ln_credit" = "Private credit"
    ),
    response_levels = c(
      "Imports", "Non-oil GDP", "Private demand", "CPI", "Private credit"
    )
  ),
  official_nsa = list(
    file = "Data/csv/timor_quarterly_official_nsa_log_and_dummys.csv",
    exo = c("shock_2016", "shock_covid", "q2", "q3", "q4"),
    suffix = "_official_nsa",
    core_vars = c("ln_gov_exp", "ln_imports", "ln_gdp_non"),
    fiscal_var = "ln_gov_exp",
    response_vars = c(
      "ln_gov_exp", "ln_imports", "ln_gdp_non",
      "ln_private_demand", "ln_gdp_ex_gov", "ln_cpi", "ln_credit"
    ),
    response_labels = c(
      "ln_gov_exp" = "Government expenditure",
      "ln_imports" = "Imports",
      "ln_gdp_non" = "Non-oil GDP",
      "ln_private_demand" = "Private demand",
      "ln_gdp_ex_gov" = "GDP excl. GFC",
      "ln_cpi" = "CPI",
      "ln_credit" = "Private credit"
    ),
    response_levels = c(
      "Imports", "Non-oil GDP", "Private demand", "GDP excl. GFC",
      "CPI", "Private credit"
    )
  ),
  long = list(
    file = "Data/csv/timor_quarterly_log_and_dummys.csv",
    exo = c(
      "shock_2006", "shock_2009", "shock_2012",
      "shock_2016", "shock_covid"
    ),
    suffix = "_long",
    core_vars = c("ln_gov_exp", "ln_imports", "ln_gdp_non"),
    fiscal_var = "ln_gov_exp",
    response_vars = c(
      "ln_gov_exp", "ln_imports", "ln_gdp_non", "ln_cpi", "ln_credit"
    ),
    response_labels = c(
      "ln_gov_exp" = "Government expenditure",
      "ln_imports" = "Imports",
      "ln_gdp_non" = "Non-oil GDP",
      "ln_cpi" = "CPI",
      "ln_credit" = "Private credit"
    ),
    response_levels = c(
      "Imports", "Non-oil GDP", "CPI", "Private credit"
    )
  ),
  stop("Unknown SAMPLE. Use official, official_broad, official_broad_dp, official_nsa, or long.")
)

data <- readr::read_csv(config$file, show_col_types = FALSE)
exo <- data %>%
  dplyr::select(dplyr::all_of(config$exo)) %>%
  as.matrix()

H <- 12
LAGS <- 2
NW_LAG <- 6
CONF_90 <- stats::qnorm(0.95)

core_vars <- config$core_vars
response_vars <- config$response_vars
fiscal_var <- config$fiscal_var

#reduced-form VAR and recursively identified fiscal shock

Y <- data %>%
  dplyr::select(dplyr::all_of(core_vars)) %>%
  as.matrix()

T <- nrow(Y)
t0 <- LAGS + 1
tt <- t0:T
n_used <- length(tt)

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
eps <- U %*% t(solve(A))

shock_raw <- as.numeric(eps[, 1])
sd_shock <- stats::sd(shock_raw)
shock <- shock_raw / sd_shock

#local projections with lpirfs

shock_full <- data.frame(
  shock = c(rep(NA_real_, LAGS), shock)
)

extract_lp <- function(fit, responses) {
  purrr::map_dfr(seq_along(responses), function(i) {
    irf_mean <- as.numeric(fit$irf_lin_mean[i, ])
    se_90 <- (as.numeric(fit$irf_lin_up[i, ]) - irf_mean) / CONF_90

    tibble::tibble(
      response = responses[i],
      horizon = 0:H,
      irf = irf_mean,
      se = se_90,
      n_obs = n_used - (0:H)
    )
  })
}

core_fit <- lpirfs::lp_lin_iv(
  endog_data = data %>%
    dplyr::select(dplyr::all_of(core_vars)) %>%
    as.data.frame(),
  shock = shock_full,
  lags_endog_lin = LAGS,
  trend = 0,
  confint = CONF_90,
  use_nw = TRUE,
  nw_lag = NW_LAG,
  nw_prewhite = FALSE,
  adjust_se = TRUE,
  hor = H + 1,
  contemp_data = exo %>%
    as.data.frame()
)

core_results <- extract_lp(core_fit, core_vars)

aux_vars <- setdiff(response_vars, core_vars)

aux_results <- purrr::map_dfr(aux_vars, function(v) {
  aux_fit <- lpirfs::lp_lin_iv(
    endog_data = data %>%
      dplyr::select(dplyr::all_of(v)) %>%
      as.data.frame(),
    shock = shock_full,
    lags_endog_lin = 0,
    exog_data = data %>%
      dplyr::select(dplyr::all_of(core_vars)) %>%
      as.data.frame(),
    lags_exog = LAGS,
    trend = 0,
    confint = CONF_90,
    use_nw = TRUE,
    nw_lag = NW_LAG,
    nw_prewhite = FALSE,
    adjust_se = TRUE,
    hor = H + 1,
    contemp_data = exo %>%
      as.data.frame()
  )

  extract_lp(aux_fit, v)
})

lp_results <- dplyr::bind_rows(core_results, aux_results) %>%
  dplyr::arrange(match(response, response_vars), horizon)

#normalise so that the impact response of government expenditure is 1%.
impact_gov <- lp_results %>%
  dplyr::filter(response == fiscal_var, horizon == 0) %>%
  dplyr::pull(irf)

if (length(impact_gov) != 1 || !is.finite(impact_gov) || impact_gov <= 0) {
  stop("Could not recover a valid positive impact response for government expenditure.")
}

lp_results <- lp_results %>%
  dplyr::mutate(
    scale = 0.01 / impact_gov,
    irf_1pct = irf * scale,
    se_1pct = se * scale,
    lower68 = pmin(irf_1pct - se_1pct, irf_1pct + se_1pct),
    upper68 = pmax(irf_1pct - se_1pct, irf_1pct + se_1pct),
    lower90 = pmin(
      irf_1pct - 1.645 * se_1pct,
      irf_1pct + 1.645 * se_1pct
    ),
    upper90 = pmax(
      irf_1pct - 1.645 * se_1pct,
      irf_1pct + 1.645 * se_1pct
    ),
    response_label = dplyr::recode(response, !!!config$response_labels)
  )

readr::write_csv(
  lp_results,
  paste0("Data/csv/lp_irf", config$suffix, ".csv")
)

#plot

plot_data <- lp_results %>%
  dplyr::filter(response != fiscal_var) %>%
  dplyr::mutate(
    response_label = factor(
      response_label,
      levels = config$response_levels
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
    fiscal_var = fiscal_var,
    impact_gov = impact_gov,
    nw_lag = NW_LAG,
    engine = "lpirfs::lp_lin_iv"
  ),
  paste0("Data/csv/lp_diagnostics", config$suffix, ".csv")
)

message("Local projections finished for sample: ", sample_name)
print(
  lp_results %>%
    dplyr::filter(response != fiscal_var) %>%
    dplyr::select(response_label, horizon, irf_1pct, lower68, upper68) %>%
    dplyr::slice_head(n = 8)
)
