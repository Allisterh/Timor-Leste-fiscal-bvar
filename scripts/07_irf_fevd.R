rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, 
               BVAR)


#sample configuration

sample_name <- Sys.getenv("SAMPLE", "official_3var")

official_exo <- c("shock_2016", "shock_covid")
long_exo <- c(
  "shock_2006", "shock_2009", "shock_2012",
  "shock_2016", "shock_covid"
)

config <- switch(
  sample_name,
  official_3var = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = official_exo,
    suffix = "_official_3var",
    vars_1 = c("ln_gov_exp", "ln_imports", "ln_gdp_non"),
    names_1 = c("Gov Exp", "Imports", "N.O. GDP"),
    vars_2 = c("ln_gdp_non", "ln_gov_exp", "ln_imports"),
    names_2 = c("N.O. GDP", "Gov Exp", "Imports")
  ),
  official_5var = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = official_exo,
    suffix = "_official_5var",
    vars_1 = c("ln_gov_exp", "ln_imports", "ln_gdp_non", "ln_cpi", "ln_credit"),
    names_1 = c("Gov Exp", "Imports", "N.O. GDP", "CPI", "Credit"),
    vars_2 = c("ln_gdp_non", "ln_gov_exp", "ln_imports", "ln_cpi", "ln_credit"),
    names_2 = c("N.O. GDP", "Gov Exp", "Imports", "CPI", "Credit")
  ),
  official_nsa = list(
    file = "Data/csv/timor_quarterly_official_nsa_log_and_dummys.csv",
    exo = c(official_exo, "q2", "q3", "q4"),
    suffix = "_official_nsa",
    vars_1 = c("ln_gov_exp", "ln_imports", "ln_gdp_non"),
    names_1 = c("Gov Exp", "Imports", "N.O. GDP"),
    vars_2 = c("ln_gdp_non", "ln_gov_exp", "ln_imports"),
    names_2 = c("N.O. GDP", "Gov Exp", "Imports")
  ),
  long_3var = list(
    file = "Data/csv/timor_quarterly_log_and_dummys.csv",
    exo = long_exo,
    suffix = "_long_3var",
    vars_1 = c("ln_gov_exp", "ln_imports", "ln_gdp_non"),
    names_1 = c("Gov Exp", "Imports", "N.O. GDP"),
    vars_2 = c("ln_gdp_non", "ln_gov_exp", "ln_imports"),
    names_2 = c("N.O. GDP", "Gov Exp", "Imports")
  ),
  long = list(
    file = "Data/csv/timor_quarterly_log_and_dummys.csv",
    exo = long_exo,
    suffix = "_long",
    vars_1 = c("ln_gov_exp", "ln_imports", "ln_gdp_non", "ln_cpi", "ln_credit"),
    names_1 = c("Gov Exp", "Imports", "N.O. GDP", "CPI", "Credit"),
    vars_2 = c("ln_gdp_non", "ln_gov_exp", "ln_imports", "ln_cpi", "ln_credit"),
    names_2 = c("N.O. GDP", "Gov Exp", "Imports", "CPI", "Credit")
  ),
  long_3var_2dum = list(
    file = "Data/csv/timor_quarterly_log_and_dummys.csv",
    exo = official_exo,
    suffix = "_long_3var_2dum",
    vars_1 = c("ln_gov_exp", "ln_imports", "ln_gdp_non"),
    names_1 = c("Gov Exp", "Imports", "N.O. GDP"),
    vars_2 = c("ln_gdp_non", "ln_gov_exp", "ln_imports"),
    names_2 = c("N.O. GDP", "Gov Exp", "Imports")
  ),
  official_private_demand = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = official_exo,
    suffix = "_official_private_demand",
    vars_1 = c("ln_gov_exp", "ln_imports", "ln_private_demand"),
    names_1 = c("Gov Exp", "Imports", "Private Demand"),
    vars_2 = c("ln_private_demand", "ln_gov_exp", "ln_imports"),
    names_2 = c("Private Demand", "Gov Exp", "Imports")
  ),
  official_gdp_ex_gov = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = official_exo,
    suffix = "_official_gdp_ex_gov",
    vars_1 = c("ln_gov_exp", "ln_imports", "ln_gdp_ex_gov"),
    names_1 = c("Gov Exp", "Imports", "GDP excl. GFC"),
    vars_2 = c("ln_gdp_ex_gov", "ln_gov_exp", "ln_imports"),
    names_2 = c("GDP excl. GFC", "Gov Exp", "Imports")
  ),
  official_broad = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = official_exo,
    suffix = "_official_broad",
    shock_name = "Gov Broad",
    vars_1 = c("ln_gov_broad", "ln_imports", "ln_gdp_non"),
    names_1 = c("Gov Broad", "Imports", "N.O. GDP"),
    vars_2 = c("ln_gdp_non", "ln_gov_broad", "ln_imports"),
    names_2 = c("N.O. GDP", "Gov Broad", "Imports")
  ),
  official_broad_dp = list(
    file = "Data/csv/timor_quarterly_official_log_and_dummys.csv",
    exo = official_exo,
    suffix = "_official_broad_dp",
    shock_name = "Gov Broad DP",
    vars_1 = c("ln_gov_broad_dp", "ln_imports", "ln_gdp_non"),
    names_1 = c("Gov Broad DP", "Imports", "N.O. GDP"),
    vars_2 = c("ln_gdp_non", "ln_gov_broad_dp", "ln_imports"),
    names_2 = c("N.O. GDP", "Gov Broad DP", "Imports")
  ),
  stop("Unknown SAMPLE. Use official_3var, official_5var, official_nsa, long_3var, long, long_3var_2dum, official_private_demand, official_gdp_ex_gov, official_broad, or official_broad_dp.")
)

data <- readr::read_csv(config$file, show_col_types = FALSE)
exogenous <- data %>%
  dplyr::select(dplyr::all_of(config$exo)) %>%
  as.matrix()

H <- 12
N_DRAW <- as.integer(Sys.getenv("N_DRAW", "100000"))
N_BURN <- as.integer(Sys.getenv("N_BURN", "20000"))
N_THIN <- as.integer(Sys.getenv("N_THIN", "20"))

#estimation function

run_order <- function(vars, names, seed, order_label,
                      shock_name = "Gov Exp") {
  endogenous <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    as.matrix()
  colnames(endogenous) <- names

  # BVAR::bvar does not accept exogenous regressors (its `...` is unused).
  # Partial out event/seasonal dummies before estimation so that the shocks
  # are not absorbed into the endogenous dynamics.
  if (ncol(exogenous) > 0) {
    endogenous <- stats::residuals(stats::lm(endogenous ~ exogenous))
    colnames(endogenous) <- names
  }

  var_base <- as.numeric(apply(diff(endogenous), 2, var, na.rm = TRUE))
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
    data = endogenous,
    lags = 2,
    priors = priors,
    n_draw = N_DRAW,
    n_burn = N_BURN,
    n_thin = N_THIN,
    verbose = FALSE
  )

  opt_irf <- BVAR::bv_irf(
    horizon = H,
    identification = TRUE,
    fevd = TRUE
  )

  irf_obj <- BVAR::irf(
    model,
    opt_irf,
    conf_bands = c(0.10, 0.16, 0.84, 0.90)
  )

  suffix <- config$suffix
  order_suffix <- paste0(order_label, suffix)

  #point IRFs -----
  grDevices::pdf(
    paste0("graphics/irf_full_matrix_order_", order_suffix, ".pdf"),
    width = 12, height = 10
  )
  plot(irf_obj)
  grDevices::dev.off()

  grDevices::pdf(
    paste0("graphics/irf_gov_shock_order_", order_suffix, ".pdf"),
    width = 10, height = 8
  )
  plot(
    irf_obj,
    vars_impulse = shock_name,
    vars_response = setdiff(names, shock_name)
  )
  grDevices::dev.off()

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
    paste0("Data/csv/irf_full_order_", order_suffix, ".csv")
  )

  gov_df <- irf_df %>%
    dplyr::filter(Impulse == shock_name) %>%
    dplyr::arrange(Response, Horizon)

  readr::write_csv(
    gov_df,
    paste0("Data/csv/irf_gov_shock_full_order_", order_suffix, ".csv")
  )

  #cumulative IRFs -----
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
    paste0("Data/csv/irf_cumulative_order_", order_suffix, ".csv")
  )

  irf_cum_obj <- irf_obj
  irf_cum_obj$quants <- cum_q
  irf_cum_obj$irf <- cum_draws

  grDevices::pdf(
    paste0("graphics/irf_cum_full_matrix_order_", order_suffix, ".pdf"),
    width = 12, height = 10
  )
  plot(irf_cum_obj)
  grDevices::dev.off()

  grDevices::pdf(
    paste0("graphics/irf_cum_gov_shock_order_", order_suffix, ".pdf"),
    width = 10, height = 8
  )
  plot(
    irf_cum_obj,
    vars_impulse = shock_name,
    vars_response = setdiff(names, shock_name)
  )
  grDevices::dev.off()

  #FEVD ----
  fevd_obj <- BVAR::fevd(irf_obj)
  fevd_q <- fevd_obj$quants

  dimnames(fevd_q) <- list(
    Quantile = c("10%", "16%", "50%", "84%", "90%"),
    Response = names,
    Horizon = seq_len(H),
    Impulse = names
  )

  fevd_df <- as.data.frame(as.table(fevd_q)) %>%
    tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
    dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
    dplyr::arrange(Response, Horizon)

  readr::write_csv(
    fevd_df,
    paste0("Data/csv/fevd_order_", order_suffix, ".csv")
  )

  fevd_plot <- fevd_df %>%
    ggplot2::ggplot(ggplot2::aes(x = Horizon, y = `50%`, fill = Impulse)) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::facet_wrap(~ Response) +
    ggplot2::labs(y = "Proportion of Explained Variance") +
    ggplot2::theme_minimal()

  ggplot2::ggsave(
    paste0("graphics/fevd_median_order_", order_suffix, ".pdf"),
    plot = fevd_plot,
    width = 12, height = 8
  )

  #stability ---
  comp <- BVAR::companion(model, type = "mean")
  roots <- eigen(comp)$values
  max_mod <- max(Mod(roots))

  roots_df <- tibble::tibble(
    real = Re(roots),
    imaginary = Im(roots)
  )

  roots_plot <- ggplot2::ggplot(
    roots_df,
    ggplot2::aes(x = real, y = imaginary)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(size = 2.5, color = "#004c99") +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = "Real part",
      y = "Imaginary part",
      title = paste0(
        "Companion roots (max modulus = ",
        format(round(max_mod, 4), nsmall = 4),
        ")"
      )
    ) +
    ggplot2::theme_minimal(base_family = "serif")

  ggplot2::ggsave(
    paste0("graphics/stability_roots_order_", order_suffix, ".pdf"),
    plot = roots_plot,
    width = 6, height = 6
  )

  readr::write_csv(
    tibble::tibble(
      sample = sample_name,
      order = order_label,
      max_modulus = max_mod,
      n_obs = nrow(data),
      horizon = H
    ),
    paste0("Data/csv/stability_order_", order_suffix, ".csv")
  )

  invisible(list(model = model, irf = irf_obj, fevd = fevd_obj))
}

#order 1: government expenditure first

order_1 <- run_order(
  vars = config$vars_1,
  names = config$names_1,
  seed = 54973997,
  order_label = "1",
  shock_name = if (is.null(config$shock_name)) "Gov Exp" else config$shock_name
)

#order 2: non-oil GDP first

order_2 <- run_order(
  vars = config$vars_2,
  names = config$names_2,
  seed = 54973998,
  order_label = "2",
  shock_name = if (is.null(config$shock_name)) "Gov Exp" else config$shock_name
)

message("BVAR finished for sample: ", sample_name)
