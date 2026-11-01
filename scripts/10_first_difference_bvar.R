rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, BVAR)

# First-difference BVAR: the same three-variable system as the baseline, but
# estimated in log differences. Cumulative IRFs are rescaled to a 1% increase
# in the level of government expenditure.

H <- 12
N_DRAW <- as.integer(Sys.getenv("N_DRAW", "30000"))
N_BURN <- as.integer(Sys.getenv("N_BURN", "6000"))
N_THIN <- as.integer(Sys.getenv("N_THIN", "6"))
LAGS <- 2

official_exo <- c("shock_2016", "shock_covid")
long_exo <- c(
  "shock_2006", "shock_2009", "shock_2012",
  "shock_2016", "shock_covid"
)

samples <- list(
  official = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = official_exo,
    suffix = "_official_3var",
    sample_label = "Official 2014-2023"
  ),
  long = list(
    file = "Data/csv/timor_quarterly_log_and_dummys.csv",
    exo = long_exo,
    suffix = "_long_3var",
    sample_label = "Long 2002-2023"
  ),
  long_2dum = list(
    file = "Data/csv/timor_quarterly_log_and_dummys.csv",
    exo = official_exo,
    suffix = "_long_3var_2dum",
    sample_label = "Long 2002-2023, 2 dummies"
  ),
  pre_2014 = list(
    file = "Data/csv/timor_quarterly_log_and_dummys.csv",
    exo = c("shock_2006", "shock_2009", "shock_2012"),
    suffix = "_long_pre_2014_3var",
    sample_label = "Long 2002-2013"
  )
)

run_first_difference <- function(cfg, sample_name, vars, names, seed, order_label) {
  data <- readr::read_csv(cfg$file, show_col_types = FALSE)

  if (sample_name == "pre_2014") {
    data <- data %>% dplyr::filter(date < as.Date("2014-01-01"))
  }

  levels <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    as.matrix()
  colnames(levels) <- names

  exo <- data %>%
    dplyr::select(dplyr::all_of(cfg$exo)) %>%
    as.matrix()

  Y <- diff(levels)
  exo_diff <- exo[-1, , drop = FALSE]

  # BVAR::bvar ignores `exogen`; partial out dummies before estimation.
  if (ncol(exo_diff) > 0) {
    Y <- stats::residuals(stats::lm(Y ~ exo_diff))
    colnames(Y) <- names
  }

  var_base <- as.numeric(apply(Y, 2, var, na.rm = TRUE))
  var_base <- pmax(var_base, 1e-5, na.rm = TRUE)

  priors <- BVAR::bv_priors(
    hyper = "lambda",
    mn = BVAR::bv_minnesota(
      lambda = BVAR::bv_lambda(mode = 0.2, min = 0.001, max = 3),
      alpha = BVAR::bv_alpha(mode = 2, min = 1, max = 3),
      psi = BVAR::bv_psi(
        mode = sqrt(var_base),
        min = sqrt(var_base) / 100,
        max = sqrt(var_base) * 100
      )
    )
  )

  set.seed(seed)
  model <- BVAR::bvar(
    data = Y,
    lags = LAGS,
    priors = priors,
    n_draw = N_DRAW,
    n_burn = N_BURN,
    n_thin = N_THIN,
    verbose = FALSE
  )

  irf_obj <- BVAR::irf(
    model,
    BVAR::bv_irf(horizon = H, identification = TRUE, fevd = FALSE),
    conf_bands = c(0.10, 0.16, 0.84, 0.90)
  )

  q <- irf_obj$quants
  dimnames(q) <- list(
    Quantile = c("10%", "16%", "50%", "84%", "90%"),
    Response = names,
    Horizon = seq_len(H),
    Impulse = names
  )

  irf_df <- as.data.frame(as.table(q)) %>%
    tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
    dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
    dplyr::arrange(Response, Horizon)

  suffix <- paste0("_", order_label, cfg$suffix, "_diff")
  readr::write_csv(irf_df, paste0("Data/csv/irf_full_order", suffix, ".csv"))

  draws <- irf_obj$irf
  dimnames(draws) <- list(
    Draw = as.character(seq_len(dim(draws)[1])),
    Response = names,
    Horizon = seq_len(H),
    Impulse = names
  )

  cum_draws <- apply(draws, MARGIN = c(1, 2, 4), FUN = cumsum)
  cum_draws <- aperm(cum_draws, c(2, 3, 1, 4))

  cum_q <- apply(
    cum_draws,
    MARGIN = c(2, 3, 4),
    FUN = quantile,
    probs = c(0.10, 0.16, 0.50, 0.84, 0.90),
    na.rm = TRUE
  )

  dimnames(cum_q) <- list(
    Quantile = c("10%", "16%", "50%", "84%", "90%"),
    Response = names,
    Horizon = seq_len(H),
    Impulse = names
  )

  cum_df <- as.data.frame(as.table(cum_q)) %>%
    tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
    dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
    dplyr::arrange(Response, Horizon)

  readr::write_csv(
    cum_df,
    paste0("Data/csv/irf_cumulative_order", suffix, ".csv")
  )

  gov_cum_impact <- stats::median(
    cum_draws[, "Gov Exp", 1, "Gov Exp"],
    na.rm = TRUE
  )
  if (!is.finite(gov_cum_impact) || gov_cum_impact <= 0) {
    stop("Could not recover a positive government cumulative impact for ", sample_name, ".")
  }

  scale <- 0.01 / gov_cum_impact
  cum_scaled <- cum_draws * scale

  cum_scaled_q <- apply(
    cum_scaled,
    MARGIN = c(2, 3, 4),
    FUN = quantile,
    probs = c(0.10, 0.16, 0.50, 0.84, 0.90),
    na.rm = TRUE
  )

  dimnames(cum_scaled_q) <- list(
    Quantile = c("10%", "16%", "50%", "84%", "90%"),
    Response = names,
    Horizon = seq_len(H),
    Impulse = names
  )

  cum_scaled_df <- as.data.frame(as.table(cum_scaled_q)) %>%
    tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
    dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
    dplyr::arrange(Response, Horizon)

  readr::write_csv(
    cum_scaled_df,
    paste0("Data/csv/irf_cumulative_scaled_order", suffix, ".csv")
  )

  comp <- BVAR::companion(model, type = "mean")
  max_mod <- max(Mod(eigen(comp)$values))

  readr::write_csv(
    tibble::tibble(
      sample = sample_name,
      sample_label = cfg$sample_label,
      order = order_label,
      max_modulus = max_mod,
      n_obs = nrow(Y),
      horizon = H
    ),
    paste0("Data/csv/stability_order", suffix, ".csv")
  )

  cum_scaled_df %>%
    dplyr::filter(
      Impulse == "Gov Exp",
      Response %in% c("Imports", "N.O. GDP"),
      Horizon %in% c(1, 3, 6, 12)
    ) %>%
    dplyr::transmute(
      sample = sample_name,
      sample_label = cfg$sample_label,
      order = order_label,
      response = Response,
      horizon = Horizon - 1,
      median_pct = `50%` * 100,
      lower68_pct = `16%` * 100,
      upper68_pct = `84%` * 100,
      lower90_pct = `10%` * 100,
      upper90_pct = `90%` * 100,
      max_modulus = max_mod,
      n_obs = nrow(Y)
    )
}

order_config <- list(
  "1" = list(
    vars = c("ln_gov_exp", "ln_imports", "ln_gdp_non"),
    names = c("Gov Exp", "Imports", "N.O. GDP"),
    seed = 54973997
  ),
  "2" = list(
    vars = c("ln_gdp_non", "ln_gov_exp", "ln_imports"),
    names = c("N.O. GDP", "Gov Exp", "Imports"),
    seed = 54973998
  )
)

summary_rows <- purrr::map_dfr(names(samples), function(sample_name) {
  cfg <- samples[[sample_name]]
  purrr::map_dfr(c("1", "2"), function(o) {
    oc <- order_config[[o]]
    run_first_difference(
      cfg = cfg,
      sample_name = sample_name,
      vars = oc$vars,
      names = oc$names,
      seed = oc$seed,
      order_label = o
    )
  })
})

readr::write_csv(
  summary_rows,
  "Data/csv/bvar_diff_summary.csv"
)

message("First-difference BVAR finished.")
print(
  summary_rows %>%
    dplyr::filter(order == "1", response == "Imports") %>%
    dplyr::select(sample, horizon, median_pct, lower68_pct, upper68_pct)
)
