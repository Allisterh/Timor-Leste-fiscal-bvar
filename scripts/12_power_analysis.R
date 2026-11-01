rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse)

# Power of the official-sample local-projection test. The simulation is
# conditional on the observed shock and controls, and adds a known response
# to the imports equation. It therefore measures the ability of the LP
# regression to detect an effect of a given size, not the power of the full
# VAR system.

LAGS <- 2
H <- 12
NW_LAG <- 6
N_REPS <- 1000
TARGETS <- c(0, 1.2, 2.1, 5.5)
HORIZONS <- c(3, 6)
SEED <- 54973997

data <- readr::read_csv(
  "Data/csv/timor_quarterly_official_log_and_dummys.csv",
  show_col_types = FALSE
)

core_vars <- c("ln_gov_exp", "ln_imports", "ln_gdp_non")
Y <- as.matrix(data[, core_vars])
exo <- as.matrix(data[, c("shock_2016", "shock_covid")])

tt <- (LAGS + 1):nrow(Y)
X <- matrix(1, nrow = length(tt), ncol = 1)
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
shock <- as.numeric(eps[, 1])
impact_gov_raw <- A[1, 1]

hac_se <- function(model, lag = NW_LAG) {
  Xm <- stats::model.matrix(model)
  e <- stats::resid(model)
  n <- nrow(Xm)
  Xe <- Xm * e
  meat <- crossprod(Xe)

  for (l in seq_len(lag)) {
    w <- 1 - l / (lag + 1)
    left <- Xm[(l + 1):n, , drop = FALSE] * e[(l + 1):n]
    right <- Xm[1:(n - l), , drop = FALSE] * e[1:(n - l)]
    gamma <- crossprod(left, right)
    meat <- meat + w * (gamma + t(gamma))
  }

  bread <- solve(crossprod(Xm))
  vcov <- bread %*% meat %*% bread
  n_adj <- n / (n - ncol(Xm))
  sqrt(diag(vcov) * n_adj)
}

simulate_t <- function(h, target_pct, reps = N_REPS) {
  idx <- seq_len(nrow(X) - h)
  y <- Y[tt[idx] + h, "ln_imports"]
  shock_h <- shock[idx]
  controls <- X[idx, -1, drop = FALSE]

  full_data <- data.frame(y = y, shock = shock_h, controls)
  full_fit <- stats::lm(y ~ ., data = full_data)
  beta_hat <- stats::coef(full_fit)[2]
  fitted_null <- stats::fitted(full_fit) - beta_hat * shock_h
  resid_null <- stats::resid(full_fit)

  beta <- target_pct * impact_gov_raw
  t_stats <- numeric(reps)
  se_vec <- numeric(reps)

  set.seed(SEED + 1000 * h + round(10 * target_pct))
  for (r in seq_len(reps)) {
    y_sim <- fitted_null + beta * shock_h + sample(resid_null, replace = TRUE)
    sim_data <- data.frame(y = y_sim, shock = shock_h, controls)
    fit <- stats::lm(y ~ ., data = sim_data)
    se <- hac_se(fit)[2]
    se_vec[r] <- se
    t_stats[r] <- stats::coef(fit)[2] / se
  }

  list(t_stats = t_stats, se_vec = se_vec)
}

power_results <- purrr::map_dfr(HORIZONS, function(h) {
  null_sim <- simulate_t(h = h, target_pct = 0)
  critical <- stats::quantile(abs(null_sim$t_stats), 0.95, names = FALSE)

  purrr::map_dfr(TARGETS, function(target) {
    if (target == 0) {
      sim <- null_sim
    } else {
      sim <- simulate_t(h = h, target_pct = target)
    }

    tibble::tibble(
      method = "LP (shock-based)",
      horizon = h,
      true_effect_pct = target,
      power = mean(abs(sim$t_stats) > critical),
      n_reps = N_REPS,
      critical_value_95 = critical,
      median_hac_se = stats::median(sim$se_vec),
      impact_gov_raw = impact_gov_raw
    )
  })
})

readr::write_csv(
  power_results,
  "Data/csv/power_analysis.csv"
)

# BVAR precision from the official posterior, for comparison with LP power.

bvar_raw <- readr::read_csv(
  "Data/csv/irf_gov_shock_full_order_1_official_3var.csv",
  show_col_types = FALSE
)

gov_impact <- bvar_raw %>%
  dplyr::filter(
    Response == "Gov Exp", Impulse == "Gov Exp", Horizon == 1
  ) %>%
  dplyr::pull(`50%`)
scale <- 0.01 / gov_impact

bvar_precision <- bvar_raw %>%
  dplyr::filter(
    Impulse == "Gov Exp",
    Response %in% c("Imports", "N.O. GDP"),
    Horizon %in% c(4, 7)
  ) %>%
  dplyr::transmute(
    method = "BVAR (hierarchical)",
    response = Response,
    horizon = Horizon - 1,
    median_pct = `50%` * scale * 100,
    lower68_pct = `16%` * scale * 100,
    upper68_pct = `84%` * scale * 100,
    se_pct = (upper68_pct - lower68_pct) / 2,
    mde_pct_95 = 1.96 * se_pct
  )

readr::write_csv(
  bvar_precision,
  "Data/csv/bvar_precision.csv"
)

message("Power analysis finished.")
print(power_results)
print(bvar_precision)
