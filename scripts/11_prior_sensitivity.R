rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, BVAR)

# Compare the Bayesian and frequentist impact responses and quantify how
# sensitive the Bayesian estimates are to the overall prior tightness.

core_vars <- c("ln_gov_exp", "ln_imports", "ln_gdp_non")
core_names <- c("Gov Exp", "Imports", "N.O. GDP")
response_names <- c("Imports", "N.O. GDP")
LAGS <- 2
H <- 12
N_DRAW <- 40000
N_BURN <- 10000
N_THIN <- 10
LAMBDA_GRID <- c(0.05, 0.10, 0.20, 0.50, 1.00, 2.00, 3.00)

official_exo <- c("shock_2016", "shock_covid")
long_exo <- c(
  "shock_2006", "shock_2009", "shock_2012",
  "shock_2016", "shock_covid"
)

samples <- list(
  official = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = official_exo,
    label = "INETL 2014-2023",
    suffix = "_official_3var"
  ),
  long = list(
    file = "Data/csv/timor_quarterly_log_and_dummys.csv",
    exo = long_exo,
    label = "Long 2002-2023",
    suffix = "_long_3var"
  ),
  long_2dum = list(
    file = "Data/csv/timor_quarterly_log_and_dummys.csv",
    exo = official_exo,
    label = "Long 2002-2023, 2 dummies",
    suffix = "_long_3var_2dum"
  )
)

make_priors <- function(Y, lambda = NULL) {
  var_base <- as.numeric(apply(diff(Y), 2, var, na.rm = TRUE))
  var_base <- pmax(var_base, 1e-5, na.rm = TRUE)

  if (is.null(lambda)) {
    BVAR::bv_priors(
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
  } else {
    BVAR::bv_priors(
      hyper = "psi",
      mn = BVAR::bv_minnesota(
        lambda = BVAR::bv_lambda(mode = lambda, min = 0.001, max = 3),
        alpha = BVAR::bv_alpha(mode = 2, min = 1, max = 3),
        psi = BVAR::bv_psi(
          mode = sqrt(var_base),
          min = sqrt(var_base) / 100,
          max = sqrt(var_base) * 100
        )
      )
    )
  }
}

extract_bvar <- function(model, sample_name, lambda_label, lambda_mode = NA_real_) {
  irf_obj <- BVAR::irf(
    model,
    BVAR::bv_irf(horizon = H, identification = TRUE, fevd = FALSE),
    conf_bands = c(0.10, 0.16, 0.84, 0.90)
  )

  q <- irf_obj$quants
  dimnames(q) <- list(
    Quantile = c("10%", "16%", "50%", "84%", "90%"),
    Response = core_names,
    Horizon = seq_len(H),
    Impulse = core_names
  )

  irf_df <- as.data.frame(as.table(q)) %>%
    tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
    dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
    dplyr::arrange(Response, Horizon)

  gov_impact <- irf_df %>%
    dplyr::filter(
      Response == "Gov Exp", Impulse == "Gov Exp", Horizon == 1
    ) %>%
    dplyr::pull(`50%`)

  if (length(gov_impact) != 1 || !is.finite(gov_impact) || gov_impact <= 0) {
    stop("Could not recover a valid government impact for ", sample_name, ".")
  }

  scale <- 0.01 / gov_impact

  comp <- BVAR::companion(model, type = "mean")
  max_mod <- max(Mod(eigen(comp)$values))

  irf_df %>%
    dplyr::filter(
      Impulse == "Gov Exp",
      Response %in% response_names,
      Horizon %in% c(1, 4, 7, 12)
    ) %>%
    dplyr::transmute(
      sample = sample_name,
      lambda_label = lambda_label,
      lambda_mode = lambda_mode,
      response = Response,
      horizon = Horizon - 1,
      median_pct = `50%` * scale * 100,
      lower68_pct = `16%` * scale * 100,
      upper68_pct = `84%` * scale * 100,
      lower90_pct = `10%` * scale * 100,
      upper90_pct = `90%` * scale * 100,
      max_modulus = max_mod,
      stable = max_mod < 1,
      n_obs = nrow(model$meta$data)
    )
}

# Hierarchical posterior of lambda.

hyper_rows <- list()
prior_rows <- list()
ols_rows <- list()

for (sample_name in names(samples)) {
  cfg <- samples[[sample_name]]
  data <- readr::read_csv(cfg$file, show_col_types = FALSE)
  Y <- as.matrix(data[, core_vars])
  colnames(Y) <- core_names
  exo <- as.matrix(data[, cfg$exo])

  # BVAR::bvar ignores `exogen`; partial out dummies before estimation.
  # The OLS-VAR benchmark below still includes the dummies explicitly.
  Y_bvar <- Y
  if (ncol(exo) > 0) {
    Y_bvar <- stats::residuals(stats::lm(Y ~ exo))
    colnames(Y_bvar) <- core_names
  }

  set.seed(54973997)
  model_hyper <- BVAR::bvar(
    data = Y_bvar,
    lags = LAGS,
    priors = make_priors(Y_bvar),
    n_draw = N_DRAW,
    n_burn = N_BURN,
    n_thin = N_THIN,
    verbose = FALSE
  )

  lambda_draws <- as.numeric(model_hyper$hyper[, 1])
  hyper_rows[[sample_name]] <- tibble::tibble(
    sample = sample_name,
    sample_label = cfg$label,
    lambda_median = stats::median(lambda_draws, na.rm = TRUE),
    lambda_q10 = stats::quantile(lambda_draws, 0.10, na.rm = TRUE),
    lambda_q90 = stats::quantile(lambda_draws, 0.90, na.rm = TRUE),
    n_draw_retained = length(lambda_draws),
    n_obs = nrow(Y)
  )

  for (lambda in LAMBDA_GRID) {
    set.seed(54973997)
    model_fixed <- BVAR::bvar(
      data = Y_bvar,
      lags = LAGS,
      priors = make_priors(Y_bvar, lambda = lambda),
      n_draw = N_DRAW,
      n_burn = N_BURN,
      n_thin = N_THIN,
      verbose = FALSE
    )

    prior_rows[[paste(sample_name, lambda, sep = "_")]] <- extract_bvar(
      model_fixed,
      sample_name = sample_name,
      lambda_label = paste0("lambda=", lambda),
      lambda_mode = lambda
    )
  }

  # OLS-VAR benchmark: same reduced-form system, Cholesky identification.

  tt <- (LAGS + 1):nrow(Y)
  X <- matrix(1, nrow = length(tt), ncol = 1)
  for (l in seq_len(LAGS)) {
    X <- cbind(X, Y[(tt - l), , drop = FALSE])
  }
  X <- cbind(X, exo[tt, , drop = FALSE])
  B <- solve(crossprod(X), crossprod(X, Y[tt, , drop = FALSE]))
  U <- Y[tt, , drop = FALSE] - X %*% B
  Sigma <- crossprod(U) / (nrow(U) - ncol(X))
  A <- t(chol(Sigma))

  k <- ncol(Y)
  phi <- array(0, dim = c(LAGS, k, k))
  for (l in seq_len(LAGS)) {
    rows <- 1 + (l - 1) * k + seq_len(k)
    phi[l, , ] <- B[rows, , drop = FALSE]
  }

  irf_arr <- array(0, dim = c(k, k, H + 1))
  irf_arr[, , 1] <- A
  for (h in seq_len(H)) {
    acc <- matrix(0, k, k)
    for (l in seq_len(min(LAGS, h))) {
      acc <- acc + phi[l, , ] %*% irf_arr[, , h - l + 1]
    }
    irf_arr[, , h + 1] <- acc
  }

  impact_gov <- irf_arr[1, 1, 1]
  if (!is.finite(impact_gov) || impact_gov <= 0) {
    stop("Invalid OLS government impact for ", sample_name, ".")
  }

  for (h in c(0, 3, 6, 11)) {
    for (r in 1:k) {
      if (core_names[r] == "Gov Exp") next
      ols_rows[[paste(sample_name, r, h, sep = "_")]] <- tibble::tibble(
        sample = sample_name,
        sample_label = cfg$label,
        estimator = "VAR-OLS",
        response = core_names[r],
        horizon = h,
        estimate_pct = irf_arr[r, 1, h + 1] / impact_gov * 1
      )
    }
  }
}

readr::write_csv(
  dplyr::bind_rows(hyper_rows),
  "Data/csv/bvar_hyperparameters.csv"
)

prior_sensitivity <- dplyr::bind_rows(prior_rows)
readr::write_csv(
  prior_sensitivity,
  "Data/csv/bvar_prior_sensitivity.csv"
)

ols_benchmark <- dplyr::bind_rows(ols_rows)
readr::write_csv(
  ols_benchmark,
  "Data/csv/var_ols_benchmark.csv"
)

# Baseline BVAR and LP values for the same horizons.

baseline_bvar <- purrr::map_dfr(c("official", "long", "long_2dum"), function(s) {
  cfg <- samples[[s]]
  path <- paste0("Data/csv/irf_gov_shock_full_order_1", cfg$suffix, ".csv")
  raw <- readr::read_csv(path, show_col_types = FALSE)
  impact <- raw %>%
    dplyr::filter(Response == "Gov Exp", Impulse == "Gov Exp", Horizon == 1) %>%
    dplyr::pull(`50%`)
  scale <- 0.01 / impact

  raw %>%
    dplyr::filter(
      Impulse == "Gov Exp",
      Response %in% response_names,
      Horizon %in% c(1, 4, 7, 12)
    ) %>%
    dplyr::transmute(
      sample = s,
      sample_label = cfg$label,
      estimator = "BVAR (hierarchical)",
      response = Response,
      horizon = Horizon - 1,
      estimate_pct = `50%` * scale * 100,
      lower68_pct = `16%` * scale * 100,
      upper68_pct = `84%` * scale * 100
    )
})

lp_files <- c(
  official = "Data/csv/lp_irf_official.csv",
  long = "Data/csv/lp_irf_long.csv"
)

baseline_lp <- purrr::map_dfr(names(lp_files), function(s) {
  raw <- readr::read_csv(lp_files[[s]], show_col_types = FALSE)
  raw %>%
    dplyr::filter(
      response %in% c("ln_imports", "ln_gdp_non"),
      horizon %in% c(0, 3, 6, 11)
    ) %>%
    dplyr::transmute(
      sample = s,
      sample_label = samples[[s]]$label,
      estimator = "LP (shock-based)",
      response = dplyr::recode(
        response,
        ln_imports = "Imports",
        ln_gdp_non = "N.O. GDP"
      ),
      horizon = horizon,
      estimate_pct = irf_1pct * 100,
      lower68_pct = lower68 * 100,
      upper68_pct = upper68 * 100
    )
})

estimator_comparison <- dplyr::bind_rows(
  baseline_bvar,
  ols_benchmark,
  baseline_lp
) %>%
  dplyr::arrange(sample, response, horizon, estimator)

readr::write_csv(
  estimator_comparison,
  "Data/csv/estimator_comparison.csv"
)

message("Prior sensitivity and OLS-VAR benchmark finished.")
print(
  estimator_comparison %>%
    dplyr::filter(horizon == 0) %>%
    dplyr::select(sample, response, estimator, estimate_pct)
)
