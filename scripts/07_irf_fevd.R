rm(list = ls())

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               BVAR
)

# import data if it doesn't exist in the environment

quarterly_log_dummy <- readr::read_csv("Data/csv/timor_quarterly_log_and_dummys.csv")
  
#series ----

endogenous <- quarterly_log_dummy %>%
  dplyr::select(
    ln_gov_exp, # 1 initial shock
    ln_imports, # 2 fiscal leaka
    ln_gdp_non, # 3 real response
    ln_cpi, # 4 price adjustment
    ln_credit # 5 financial liquidity
  ) %>%
  as.matrix()
#note: the cpi is expressed as an index
#the other variables are in millions of dollars. 

exogenous <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("shock_")) %>%
  as.matrix()

#prior lag selection and initial hyperparameters ----

# The series have unit roots and missing values; use the variance of first differences to scale the prior.
var_base <- as.numeric(apply(diff(endogenous), 2, var, na.rm = TRUE))

#pmax() force the variance to be less than 1e-5
var_base <- pmax(var_base, 1e-5, na.rm = TRUE)

priors_spec_2 <- BVAR::bv_priors(
  hyper = "lambda",
  mn = BVAR::bv_minnesota(
    # Literature standard for small/emerging economies with interpolated quarterly data:
    # Fixing/tightening lambda around 0.2 avoids overfitting the smoothness of Denton-Cholette
    lambda = BVAR::bv_lambda(mode = 0.2,
                             min = 0.001, 
                             max = 3),
    
    alpha = BVAR::bv_alpha(mode = 2, 
                           min = 1, 
                           max = 3),
    
    psi = BVAR::bv_psi(
      mode = sqrt(var_base),
      min = sqrt(var_base) / 100,  
      max = sqrt(var_base) * 100
    )
  )
)

# order cholesky 1 ----

#order 1
endogenous_order_1 <- quarterly_log_dummy %>%
  dplyr::select(
    ln_gov_exp, # 1 initial shock
    ln_imports, # 2 fiscal leaka
    ln_gdp_non, # 3 real response
    ln_cpi, # 4 price adjustment
    ln_credit # 5 financial liquidity
  ) %>%
  as.matrix()

# professional names 
prof_names_order_1 <- c("Gov Exp", 
                        "Imports", 
                        "N.O. GDP", 
                        "CPI", 
                        "Credit")

colnames(endogenous_order_1) <- prof_names_order_1

set.seed(54973997)

timor_order_1 <- BVAR::bvar(
  data = endogenous_order_1,
  lags = 2,
  exogen = exogenous,
  priors = priors_spec_2,
  n_draw = 300000, 
  n_burn = 100000,
  n_thin = 50,
  verbose = TRUE 
)

#irf ----

opt_irf_order_1 <- BVAR::bv_irf(
  horizon = 20,
  identification = TRUE, # order cholesky
  fevd = TRUE # calculated fevd 
)

irf_timor_order_1 <- BVAR::irf(timor_order_1,
                               opt_irf_order_1, 
                               conf_bands = c(0.10, 0.16, 0.84, 0.90))

pdf("graphics/irf_full_matrix_order_1.pdf", width = 12, height = 10)
plot(irf_timor_order_1)
dev.off()

pdf("graphics/irf_gov_shock_order_1.pdf", width = 10, height = 8)
plot(irf_timor_order_1, 
     vars_impulse = "Gov Exp", 
     vars_response = c("Imports", "N.O. GDP", "CPI", "Credit"))
dev.off()

#irf table ----

names(irf_timor_order_1)

irf_raw_order_1 <- irf_timor_order_1$quants
irf_df_order_1 <- as.data.frame(as.table(irf_raw_order_1))

colnames(irf_df_order_1) <- c("Quantile", "Response", "Horizon", "Impulse", "Value")
print(irf_df_order_1)

dimnames(irf_raw_order_1) <- list(
  Quantile = c("10%", "16%", "50%", "84%", "90%"), # Computes 68% (0.16-0.84) 
  #and 80% (0.10-0.90) intervals
  Response = prof_names_order_1,
  Horizon = 1:20,
  Impulse = prof_names_order_1
)

irf_df_order_1 <- as.data.frame(as.table(irf_raw_order_1))
print(irf_df_order_1)

irf_df_order_1 <- irf_df_order_1 %>%
  tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
  dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
  dplyr::arrange(Response, Horizon)

print(irf_df_order_1)

readr::write_csv(irf_df_order_1, "Data/csv/irf_full_order_1.csv")

#gov shock table
irf_gov_shock_df_order_1 <- irf_df_order_1 %>%
  dplyr::filter(Impulse == "Gov Exp") %>%
  dplyr::arrange(Response, Horizon)

readr::write_csv(irf_gov_shock_df_order_1, "Data/csv/irf_gov_shock_full_order_1.csv")

#camulative irf (CIRF) ----

irf_draws_order_1 <- irf_timor_order_1$irf
print(irf_draws_order_1)

dimnames(irf_draws_order_1) <- list(
  Draw = as.character(1:4000),
  Response = prof_names_order_1,
  Horizon = 1:20,
  Impulse = prof_names_order_1
)

print(irf_draws_order_1)

irf_cum_draws_order_1 <- apply(irf_draws_order_1, MARGIN = c(1, 2, 4), FUN = cumsum)
dim(irf_cum_draws_order_1)  

irf_cum_draws_order_1 <- aperm(irf_cum_draws_order_1, c(2, 3, 1, 4))
dim(irf_cum_draws_order_1)
print(irf_cum_draws_order_1)
identical(dim(irf_cum_draws_order_1), dim(irf_timor_order_1$irf))

irf_cum_quants_order_1 <- apply(irf_cum_draws_order_1, MARGIN = c(2, 3, 4), FUN = quantile,
                                probs = c(0.10, 0.16, 0.50, 0.84, 0.90), na.rm = TRUE)
dim(irf_cum_quants_order_1)  

identical(dim(irf_cum_quants_order_1), dim(irf_timor_order_1$quants))  

dimnames(irf_cum_quants_order_1) <- list(
  Quantile = c("10%", "16%", "50%", "84%", "90%"),
  Response = prof_names_order_1,
  Horizon = 1:20,
  Impulse = prof_names_order_1
)

print(irf_cum_quants_order_1)

#cirf plot ---
irf_cum_order_1 <- irf_timor_order_1
irf_cum_order_1$quants <- irf_cum_quants_order_1
irf_cum_order_1$irf <- irf_cum_draws_order_1

class(irf_cum_order_1) 

pdf("graphics/irf_cum_full_matrix_order_1.pdf", width = 12, height = 10)
plot(irf_cum_order_1)
dev.off()

pdf("graphics/irf_cum_gov_shock_order_1.pdf", width = 10, height = 8)
plot(irf_cum_order_1, 
     vars_impulse = "Gov Exp", 
     vars_response = c("Imports", "N.O. GDP", "CPI", "Credit"))
dev.off()

#cirf table ----

irf_cum_quants_arr_oder_1 <- apply(irf_cum_draws_order_1, MARGIN = c(2, 3, 4), FUN = quantile,
                            probs = c(0.10, 0.16, 0.50, 0.84, 0.90), na.rm = TRUE)

dim(irf_cum_quants_arr_oder_1)    
class(irf_cum_quants_arr_oder_1)  
identical(dim(irf_cum_quants_arr_oder_1), dim(irf_timor_order_1$quants))  

dimnames(irf_cum_quants_arr_oder_1) <- list(
  Quantile = c("10%", "16%", "50%", "84%", "90%"),
  Response = prof_names_order_1,
  Horizon  = 1:20,
  Impulse  = prof_names_order_1
)

irf_cum_df_order_1 <- as.data.frame(as.table(irf_cum_quants_arr_oder_1))
colnames(irf_cum_df_order_1) <- c("Quantile", "Response", "Horizon", "Impulse", "Value")

irf_cum_df_order_1 <- irf_cum_df_order_1 %>%
  tidyr::pivot_wider(names_from = Quantile, values_from = Value) %>%
  dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
  dplyr::arrange(Response, Horizon)

print(irf_cum_df_order_1)
readr::write_csv(irf_cum_df_order_1, "Data/csv/irf_cumulative_order_1.csv")

#fevd ----

fevd_timor_order_1 <- BVAR::fevd(irf_timor_order_1)

#' pdf("graphics/fevd_full_order_1.pdf", width = 12, height = 10)
#' plot(fevd_timor_order_1)
#' # Error en xy.coords(x, y, xlabel, ylabel, log):
#' #'x' is a list, but does not have components 'x' and 'y'
#' # fevd has a bug
#' dev.off()

fevd_raw_order_1 <- fevd_timor_order_1$quants

dimnames(fevd_raw_order_1) <- list(
  Quantile = c("10%", "16%", "50%", "84%", "90%"), 
  Response = prof_names_order_1,
  Horizon = 1:20,
  Impulse = prof_names_order_1
)

print(fevd_raw_order_1)

fevd_df_order_1 <- as.data.frame(as.table(fevd_raw_order_1)) %>%
  tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
  dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
  dplyr::arrange(Response, Horizon)

print(fevd_df_order_1)

fevd_median_order_1 <- fevd_df_order_1 %>%
  ggplot2::ggplot(ggplot2::aes(x = Horizon, y = `50%`, fill = Impulse)) +
  ggplot2::geom_area(position = "stack") +
  ggplot2::facet_wrap(~ Response) +
  ggplot2::labs(y = "Proportion of Explained Variance", 
                #title = "FEVD by Variables (median)") +
                ) +
  ggplot2::theme_minimal()

print(fevd_median_order_1)

ggplot2::ggsave("graphics/fevd_median_order_1.pdf", 
                plot = fevd_median_order_1, 
                width = 12, height = 8)

readr::write_csv(fevd_df_order_1, "Data/csv/fevd_order_1.csv")


## another order ----

# order cholesky 2 ----

#order 2
endogenous_order_2 <- quarterly_log_dummy %>%
  dplyr::select(
    ln_gdp_non, # 3 real response
    ln_gov_exp, # 1 initial shock 
    ln_imports, # 2 fiscal leaka
    ln_cpi, # 4 price adjustment
    ln_credit # 5 financial liquidity
  ) %>%
  as.matrix()

# professional names 
prof_names_order_2 <- c("N.O. GDP",
                        "Gov Exp", 
                        "Imports", 
                        "CPI", 
                        "Credit")

colnames(endogenous_order_2) <- prof_names_order_2

timor_order_2 <- BVAR::bvar(
  data = endogenous_order_2,
  lags = 2,
  exogen = exogenous,
  priors = priors_spec_2,
  n_draw = 300000, 
  n_burn = 100000,
  n_thin = 50,
  verbose = TRUE 
)

#irf order 2 ----

opt_irf_order_2 <- BVAR::bv_irf(
  horizon = 20,
  identification = TRUE, # order cholesky
  fevd = TRUE # calculated fevd 
)

irf_timor_order_2 <- BVAR::irf(timor_order_2,
                               opt_irf_order_2, 
                               conf_bands = c(0.10, 0.16, 0.84, 0.90))

pdf("graphics/irf_full_matrix_order_2.pdf", width = 12, height = 10)
plot(irf_timor_order_2)
dev.off()

pdf("graphics/irf_gov_shock_order_2.pdf", width = 10, height = 8)
plot(irf_timor_order_2, 
     vars_impulse = "Gov Exp", 
     vars_response = c("Imports", "N.O. GDP", "CPI", "Credit"))
dev.off()

#irf table order 2 ----

names(irf_timor_order_2)

irf_raw_order_2 <- irf_timor_order_2$quants
irf_df_order_2 <- as.data.frame(as.table(irf_raw_order_2))

colnames(irf_df_order_2) <- c("Quantile", "Response", "Horizon", "Impulse", "Value")
print(irf_df_order_2)

dimnames(irf_raw_order_2) <- list(
  Quantile = c("10%", "16%", "50%", "84%", "90%"), # Computes 68% (0.16-0.84) 
  #and 80% (0.10-0.90) intervals
  Response = prof_names_order_2,
  Horizon = 1:20,
  Impulse = prof_names_order_2
)

irf_df_order_2 <- as.data.frame(as.table(irf_raw_order_2))
print(irf_df_order_2)

irf_df_order_2 <- irf_df_order_2 %>%
  tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
  dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
  dplyr::arrange(Response, Horizon)

print(irf_df_order_2)

readr::write_csv(irf_df_order_2, "Data/csv/irf_full_order_2.csv")

#gov shock table
irf_gov_shock_df_order_2 <- irf_df_order_2 %>%
  dplyr::filter(Impulse == "Gov Exp") %>%
  dplyr::arrange(Response, Horizon)

readr::write_csv(irf_gov_shock_df_order_2, "Data/csv/irf_gov_shock_full_order_2.csv")

#camulative irf (CIRF) order 2 ----

irf_draws_order_2 <- irf_timor_order_2$irf
print(irf_draws_order_2)

dimnames(irf_draws_order_2) <- list(
  Draw = as.character(1:4000),
  Response = prof_names_order_2,
  Horizon = 1:20,
  Impulse = prof_names_order_2
)

print(irf_draws_order_2)

irf_cum_draws_order_2 <- apply(irf_draws_order_2, MARGIN = c(1, 2, 4), FUN = cumsum)
dim(irf_cum_draws_order_2)  

irf_cum_draws_order_2 <- aperm(irf_cum_draws_order_2, c(2, 3, 1, 4))
dim(irf_cum_draws_order_2)
print(irf_cum_draws_order_2)
identical(dim(irf_cum_draws_order_2), dim(irf_timor_order_2$irf))

irf_cum_quants_order_2 <- apply(irf_cum_draws_order_2, MARGIN = c(2, 3, 4), FUN = quantile,
                                probs = c(0.10, 0.16, 0.50, 0.84, 0.90), na.rm = TRUE)
dim(irf_cum_quants_order_2)  

identical(dim(irf_cum_quants_order_2), dim(irf_timor_order_2$quants))  

dimnames(irf_cum_quants_order_2) <- list(
  Quantile = c("10%", "16%", "50%", "84%", "90%"),
  Response = prof_names_order_2,
  Horizon = 1:20,
  Impulse = prof_names_order_2
)

print(irf_cum_quants_order_2)

#cirf plot ---
irf_cum_order_2 <- irf_timor_order_2
irf_cum_order_2$quants <- irf_cum_quants_order_2
irf_cum_order_2$irf <- irf_cum_draws_order_2

class(irf_cum_order_2) 

pdf("graphics/irf_cum_full_matrix_order_2.pdf", width = 12, height = 10)
plot(irf_cum_order_2)
dev.off()

pdf("graphics/irf_cum_gov_shock_order_2.pdf", width = 10, height = 8)
plot(irf_cum_order_2, 
     vars_impulse = "Gov Exp", 
     vars_response = c("Imports", "N.O. GDP", "CPI", "Credit"))
dev.off()

#cirf table order 2 ----

irf_cum_quants_arr_order_2 <- apply(irf_cum_draws_order_2, MARGIN = c(2, 3, 4), FUN = quantile,
                            probs = c(0.10, 0.16, 0.50, 0.84, 0.90), na.rm = TRUE)

dim(irf_cum_quants_arr_order_2)    
class(irf_cum_quants_arr_order_2)  
identical(dim(irf_cum_quants_arr_order_2), dim(irf_timor_order_2$quants))  

dimnames(irf_cum_quants_arr_order_2) <- list(
  Quantile = c("10%", "16%", "50%", "84%", "90%"),
  Response = prof_names_order_2,
  Horizon  = 1:20,
  Impulse  = prof_names_order_2
)

irf_cum_df_order_2 <- as.data.frame(as.table(irf_cum_quants_arr_order_2))
colnames(irf_cum_df_order_2) <- c("Quantile", "Response", "Horizon", "Impulse", "Value")

irf_cum_df_order_2 <- irf_cum_df_order_2 %>%
  tidyr::pivot_wider(names_from = Quantile, values_from = Value) %>%
  dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
  dplyr::arrange(Response, Horizon)

print(irf_cum_df_order_2)
readr::write_csv(irf_cum_df_order_2, "Data/csv/irf_cumulative_order_2.csv")

#fevd order 2 ----

fevd_timor_order_2 <- BVAR::fevd(irf_timor_order_2)

#' pdf("graphics/fevd_full_order_2.pdf", width = 12, height = 10)
#' plot(fevd_timor_order_2)
#' # Error en xy.coords(x, y, xlabel, ylabel, log):
#' #'x' is a list, but does not have components 'x' and 'y'
#' # fevd has a bug
#' dev.off()

fevd_raw_order_2 <- fevd_timor_order_2$quants

dimnames(fevd_raw_order_2) <- list(
  Quantile = c("10%", "16%", "50%", "84%", "90%"), 
  Response = prof_names_order_2,
  Horizon = 1:20,
  Impulse = prof_names_order_2
)

print(fevd_raw_order_2)

fevd_df_order_2 <- as.data.frame(as.table(fevd_raw_order_2)) %>%
  tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
  dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
  dplyr::arrange(Response, Horizon)

print(fevd_df_order_2)

fevd_median_order_2 <- fevd_df_order_2 %>%
  ggplot2::ggplot(ggplot2::aes(x = Horizon, y = `50%`, fill = Impulse)) +
  ggplot2::geom_area(position = "stack") +
  ggplot2::facet_wrap(~ Response) +
  ggplot2::labs(y = "Proportion of Explained Variance", 
                #title = "FEVD by Variables (median)") +
  ) +
  ggplot2::theme_minimal()

print(fevd_median_order_2)

ggplot2::ggsave("graphics/fevd_median_order_2.pdf", 
                plot = fevd_median_order_2, 
                width = 12, height = 8)

readr::write_csv(fevd_df_order_2, "Data/csv/fevd_order_2.csv")


