rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, 
               BVAR)

#Hybrid robustness: broader ADB government expenditure (interpolated) with
#official quarterly outcomes for GDP and imports. The purpose is to check
#whether the absence of leakage in the official sample is due to the narrower
#national-accounts measure of government final consumption expenditure.

official <- readr::read_csv(
  "Data/csv/timor_quarterly_official_log_and_dummys.csv",
  show_col_types = FALSE
)

long_gov <- readr::read_csv(
  "Data/csv/timor_quarterly_data.csv",
  show_col_types = FALSE
) %>%
  dplyr::select(date, gov_adb = gov_expenditure_real)

hybrid <- official %>%
  dplyr::left_join(long_gov, by = "date") %>%
  dplyr::mutate(ln_gov_exp = log(gov_adb)) %>%
  dplyr::select(-gov_adb)

readr::write_csv(
  hybrid,
  "Data/csv/timor_quarterly_hybrid_log_and_dummys.csv"
)

vars <- c("ln_gov_exp", "ln_imports", "ln_gdp_non")
names <- c("Gov Exp", "Imports", "N.O. GDP")

Y <- hybrid %>%
  dplyr::select(dplyr::all_of(vars)) %>%
  as.matrix()
colnames(Y) <- names

exo <- hybrid %>%
  dplyr::select(shock_2016, shock_covid) %>%
  as.matrix()

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

set.seed(54973997)
model <- BVAR::bvar(
  data = Y,
  lags = 2,
  exogen = exo,
  priors = priors,
  n_draw = 100000,
  n_burn = 20000,
  n_thin = 20,
  verbose = FALSE
)

H <- 12
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

readr::write_csv(
  irf_df,
  "Data/csv/irf_full_order_1_hybrid_3var.csv"
)

readr::write_csv(
  irf_df %>% dplyr::filter(Impulse == "Gov Exp"),
  "Data/csv/irf_gov_shock_full_order_1_hybrid_3var.csv"
)

comp <- BVAR::companion(model, type = "mean")
readr::write_csv(
  tibble::tibble(
    sample = "hybrid_3var",
    order = "1",
    max_modulus = max(Mod(eigen(comp)$values)),
    n_obs = nrow(Y),
    horizon = H
  ),
  "Data/csv/stability_order_1_hybrid_3var.csv"
)

