rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               BVAR)

#Hybrid 2x2 design on the common 2014-2023 window. The four cells vary the
#fiscal series (official narrow vs. interpolated ADB broad) and the outcome
#series (official vs. interpolated), holding the sample, lags, prior, and
#identification fixed. Order 1 is the main specification; order 2 is reported
#as an appendix robustness check.

official <- readr::read_csv(
  "Data/csv/timor_quarterly_official_log_and_dummys.csv",
  show_col_types = FALSE
)

long <- readr::read_csv(
  "Data/csv/timor_quarterly_log_and_dummys.csv",
  show_col_types = FALSE
)

long_2014 <- long %>%
  dplyr::filter(date >= as.Date("2014-01-01")) %>%
  dplyr::select(
    date, ln_gov_exp, ln_imports, ln_gdp_non, ln_cpi, ln_credit,
    shock_2016, shock_covid
  )

long_pre_2014 <- long %>%
  dplyr::filter(date < as.Date("2014-01-01")) %>%
  dplyr::select(
    date, ln_gov_exp, ln_imports, ln_gdp_non, ln_cpi, ln_credit,
    shock_2006, shock_2009, shock_2012, shock_2016, shock_covid
  )

hybrid_fiscal <- official %>%
  dplyr::select(
    date, ln_imports, ln_gdp_non, ln_cpi, ln_credit,
    shock_2016, shock_covid
  ) %>%
  dplyr::left_join(
    long %>% dplyr::select(date, ln_gov_exp),
    by = "date"
  ) %>%
  dplyr::select(
    date, ln_gov_exp, ln_imports, ln_gdp_non, ln_cpi, ln_credit,
    shock_2016, shock_covid
  ) %>%
  dplyr::arrange(date)

hybrid_outcomes <- official %>%
  dplyr::select(date, ln_gov_exp, shock_2016, shock_covid) %>%
  dplyr::left_join(
    long %>% dplyr::select(
      date, ln_imports, ln_gdp_non, ln_cpi, ln_credit
    ),
    by = "date"
  ) %>%
  dplyr::select(
    date, ln_gov_exp, ln_imports, ln_gdp_non, ln_cpi, ln_credit,
    shock_2016, shock_covid
  ) %>%
  dplyr::arrange(date)

readr::write_csv(
  hybrid_fiscal,
  "Data/csv/timor_quarterly_hybrid_log_and_dummys.csv"
)

readr::write_csv(
  hybrid_outcomes,
  "Data/csv/timor_quarterly_hybrid_outcomes_log_and_dummys.csv"
)

readr::write_csv(
  long_2014,
  "Data/csv/timor_quarterly_long_2014_log_and_dummys.csv"
)

readr::write_csv(
  long_pre_2014,
  "Data/csv/timor_quarterly_long_pre_2014_log_and_dummys.csv"
)

H <- 12
N_DRAW <- as.integer(Sys.getenv("N_DRAW", "100000"))
N_BURN <- as.integer(Sys.getenv("N_BURN", "20000"))
N_THIN <- as.integer(Sys.getenv("N_THIN", "20"))

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

estimate_cell <- function(cell, fiscal, outcomes, data, suffix, order_label,
                          exo_vars = c("shock_2016", "shock_covid"),
                          sample_label = "2014-2023") {
  cfg <- order_config[[order_label]]

  Y <- data %>%
    dplyr::select(dplyr::all_of(cfg$vars)) %>%
    as.matrix()
  colnames(Y) <- cfg$names

  exo <- data %>%
    dplyr::select(dplyr::all_of(exo_vars)) %>%
    as.matrix()

  # BVAR::bvar ignores `exogen`; partial out dummies before estimation.
  if (ncol(exo) > 0) {
    Y <- stats::residuals(stats::lm(Y ~ exo))
    colnames(Y) <- cfg$names
  }

  var_base <- pmax(apply(diff(Y), 2, var, na.rm = TRUE), 1e-5)

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

  set.seed(cfg$seed)
  model <- BVAR::bvar(
    data = Y,
    lags = 2,
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
    Response = cfg$names,
    Horizon = seq_len(H),
    Impulse = cfg$names
  )

  irf_df <- as.data.frame(as.table(q)) %>%
    tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
    dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
    dplyr::arrange(Response, Horizon)

  gov_impact <- irf_df %>%
    dplyr::filter(
      Response == "Gov Exp",
      Impulse == "Gov Exp",
      Horizon == 1
    ) %>%
    dplyr::pull(`50%`)

  if (length(gov_impact) != 1 || !is.finite(gov_impact) || gov_impact <= 0) {
    stop("Could not recover a valid government impact for ", cell, ", order ", order_label, ".")
  }

  scale_1pct <- 0.01 / gov_impact

  readr::write_csv(
    irf_df,
    paste0("Data/csv/irf_full_order_", order_label, suffix, ".csv")
  )

  readr::write_csv(
    irf_df %>% dplyr::filter(Impulse == "Gov Exp"),
    paste0(
      "Data/csv/irf_gov_shock_full_order_", order_label, suffix, ".csv"
    )
  )

  comp <- BVAR::companion(model, type = "mean")
  max_mod <- max(Mod(eigen(comp)$values))

  stability <- tibble::tibble(
    sample = paste0(cell, "_", order_label),
    order = order_label,
    max_modulus = max_mod,
    n_obs = nrow(Y),
    horizon = H
  )

  readr::write_csv(
    stability,
    paste0("Data/csv/stability_order_", order_label, suffix, ".csv")
  )

  irf_df %>%
    dplyr::filter(
      Impulse == "Gov Exp",
      Response %in% c("Imports", "N.O. GDP"),
      Horizon %in% c(1, 3, 6, 12)
    ) %>%
    dplyr::transmute(
      cell = cell,
      fiscal = fiscal,
      outcomes = outcomes,
      sample = sample_label,
      order = order_label,
      response = Response,
      horizon = Horizon,
      median_pct = `50%` * scale_1pct * 100,
      lower68_pct = `16%` * scale_1pct * 100,
      upper68_pct = `84%` * scale_1pct * 100,
      lower90_pct = `10%` * scale_1pct * 100,
      upper90_pct = `90%` * scale_1pct * 100,
      max_modulus = max_mod,
      stable = max_mod < 1,
      n_obs = nrow(Y)
    )
}

new_cells <- dplyr::bind_rows(
  purrr::map_dfr(c("1", "2"), function(o) {
    estimate_cell(
      cell = "Hybrid-F",
      fiscal = "ADB broad interpolated",
      outcomes = "Official",
      data = hybrid_fiscal,
      suffix = "_hybrid_3var",
      order_label = o
    )
  }),
  purrr::map_dfr(c("1", "2"), function(o) {
    estimate_cell(
      cell = "Hybrid-O",
      fiscal = "Official narrow",
      outcomes = "Interpolated",
      data = hybrid_outcomes,
      suffix = "_hybrid_outcomes_3var",
      order_label = o
    )
  }),
  purrr::map_dfr(c("1", "2"), function(o) {
    estimate_cell(
      cell = "Interpolated both",
      fiscal = "ADB broad interpolated",
      outcomes = "Interpolated",
      data = long_2014,
      suffix = "_long_2014_3var",
      order_label = o
    )
  })
)

read_cell <- function(path_irf, path_stability, cell, fiscal, outcomes, order,
                      sample_label = "2014-2023") {
  irf <- readr::read_csv(path_irf, show_col_types = FALSE)
  stability <- readr::read_csv(path_stability, show_col_types = FALSE)

  gov_impact <- irf %>%
    dplyr::filter(
      Response == "Gov Exp",
      Impulse == "Gov Exp",
      Horizon == 1
    ) %>%
    dplyr::pull(`50%`)

  if (length(gov_impact) != 1 || !is.finite(gov_impact) || gov_impact <= 0) {
    stop("Could not recover a valid government impact from ", path_irf, ".")
  }

  scale_1pct <- 0.01 / gov_impact

  irf %>%
    dplyr::filter(
      Impulse == "Gov Exp",
      Response %in% c("Imports", "N.O. GDP"),
      Horizon %in% c(1, 3, 6, 12)
    ) %>%
    dplyr::transmute(
      cell = cell,
      fiscal = fiscal,
      outcomes = outcomes,
      sample = sample_label,
      order = order,
      response = Response,
      horizon = Horizon,
      median_pct = `50%` * scale_1pct * 100,
      lower68_pct = `16%` * scale_1pct * 100,
      upper68_pct = `84%` * scale_1pct * 100,
      lower90_pct = `10%` * scale_1pct * 100,
      upper90_pct = `90%` * scale_1pct * 100,
      max_modulus = stability$max_modulus[1],
      stable = stability$max_modulus[1] < 1,
      n_obs = stability$n_obs[1]
    )
}

baseline_cells <- dplyr::bind_rows(
  purrr::map_dfr(c("1", "2"), function(o) {
    read_cell(
      path_irf = paste0(
        "Data/csv/irf_gov_shock_full_order_", o, "_official_3var.csv"
      ),
      path_stability = paste0(
        "Data/csv/stability_order_", o, "_official_3var.csv"
      ),
      cell = "Official baseline",
      fiscal = "Official narrow",
      outcomes = "Official",
      order = o
    )
  })
)

hybrid_2x2 <- dplyr::bind_rows(baseline_cells, new_cells) %>%
  dplyr::arrange(order, factor(cell, levels = c(
    "Official baseline", "Hybrid-F", "Hybrid-O", "Interpolated both"
  )), response, horizon)

readr::write_csv(
  hybrid_2x2,
  "Data/csv/hybrid_2x2_summary.csv"
)

long_exo <- c(
  "shock_2006", "shock_2009", "shock_2012", "shock_2016", "shock_covid"
)

pre_cells <- purrr::map_dfr(c("1", "2"), function(o) {
  estimate_cell(
    cell = "Interpolated 2002-2013",
    fiscal = "ADB broad interpolated",
    outcomes = "Interpolated",
    data = long_pre_2014,
    suffix = "_long_pre_2014_3var",
    order_label = o,
    exo_vars = long_exo,
    sample_label = "2002-2013"
  )
})

long_full_cells <- purrr::map_dfr(c("1", "2"), function(o) {
  read_cell(
    path_irf = paste0(
      "Data/csv/irf_gov_shock_full_order_", o, "_long_3var.csv"
    ),
    path_stability = paste0(
      "Data/csv/stability_order_", o, "_long_3var.csv"
    ),
    cell = "Interpolated 2002-2023",
    fiscal = "ADB broad interpolated",
    outcomes = "Interpolated",
    order = o,
    sample_label = "2002-2023"
  )
})

long_2014_cells <- new_cells %>%
  dplyr::filter(cell == "Interpolated both") %>%
  dplyr::mutate(
    cell = "Interpolated 2014-2023",
    sample = "2014-2023"
  )

long_2dum_cells <- purrr::map_dfr(c("1", "2"), function(o) {
  read_cell(
    path_irf = paste0(
      "Data/csv/irf_gov_shock_full_order_", o, "_long_3var_2dum.csv"
    ),
    path_stability = paste0(
      "Data/csv/stability_order_", o, "_long_3var_2dum.csv"
    ),
    cell = "Interpolated 2002-2023, 2 dummies",
    fiscal = "ADB broad interpolated",
    outcomes = "Interpolated",
    order = o,
    sample_label = "2002-2023"
  )
})

period_decomposition <- dplyr::bind_rows(
  baseline_cells,
  long_2014_cells,
  pre_cells,
  long_full_cells,
  long_2dum_cells
) %>%
  dplyr::arrange(
    order,
    factor(cell, levels = c(
      "Official baseline",
      "Interpolated 2014-2023",
      "Interpolated 2002-2013",
      "Interpolated 2002-2023",
      "Interpolated 2002-2023, 2 dummies"
    )),
    response,
    horizon
  )

readr::write_csv(
  period_decomposition,
  "Data/csv/period_decomposition_summary.csv"
)

message("Hybrid 2x2 robustness and period decomposition finished.")
print(
  hybrid_2x2 %>%
    dplyr::filter(order == "1", response == "Imports") %>%
    dplyr::select(cell, horizon, median_pct, lower68_pct, upper68_pct, stable)
)
