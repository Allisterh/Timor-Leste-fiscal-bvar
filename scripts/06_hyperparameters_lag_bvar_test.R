if(F){
  "
  
  the series has few data point (88) and has been disaggregated i don't trust
  the result, so be careful with estimates
  
  "
}

rm(list = ls())

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               BVAR,
               coda, #for the Geweke convergence test of mcmc
               FinTS #for the ARCH test of heteroscedasticity
)

# import data if it doesn't exist in the environment

quarterly_log_dummy <- readr::read_csv("Data/csv/timor_quarterly_log_and_dummys.csv")

#series ----

endogenous <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("ln_")) %>%
  as.matrix()

exogenous <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("shock_")) %>%
  as.matrix()

#prior 1 lag selection and initial hyperparameters ----

#it does 2 diferences, because the series has a unit root so arima can't with na
var_base <- as.numeric(apply(diff(endogenous), 2, var, na.rm = TRUE))

#pmax() force the variance to be less than 1e-5
var_base <- pmax(var_base, 1e-5, na.rm = TRUE)

priors_spec <- BVAR::bv_priors(
  hyper = "auto",
  mn = BVAR::bv_minnesota(
    lambda = BVAR::bv_lambda(mode = 0.2, min = 0.0001, max = 1.5),
    alpha = BVAR::bv_alpha(mode = 2, min = 1, max = 3),
    # psi = BVAR::bv_psi() 
    psi = BVAR::bv_psi(
      mode = sqrt(var_base),
      min = sqrt(var_base) / 100,
      max = sqrt(var_base) * 100
    )
    )
)

#the lag criterion is based on maximum likelihood
table_lags_bvar <- data.frame()

for (p_i in 1:12) {
  set.seed(54973997)
  
  mod_tmp <- BVAR::bvar(
    data = endogenous,
    lags = p_i,
    exogen = exogenous,
    priors = priors_spec,
    n_draw = 10000,
    n_burn = 1000,
    verbose = FALSE
  )
  
  #get the log-marginal likelihood
  lml_val <- logLik(mod_tmp)
  
  table_lags_bvar <- rbind(table_lags_bvar, data.frame(
    lag_p = p_i,
    Log_Marginal_Likelihood = round(as.numeric(lml_val), 2)
  ))
}

print(table_lags_bvar)

#results
# lag_p Log_Marginal_Likelihood
# 1      1                  941.19
# 2      2                 1060.01
# 3      3                 1064.12 winner
# 4      4                 1048.30
# 5      5                 1051.82
# 6      6                 1048.13
# 7      7                 1039.70
# 8      8                 1021.15
# 9      9                 1001.90
# 10    10                  980.15
# 11    11                  970.32
# 12    12                  942.02

#prior 2 lambda sensitivity----

lambda_grid <- c(0.1 ,0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.4, 2.9 , 3)
table_lambda_sensitivity <- data.frame()

for (lam in lambda_grid) {
  priors_tmp <- BVAR::bv_priors(
    hyper = "auto",
    mn = BVAR::bv_minnesota(
      lambda = BVAR::bv_lambda(mode = lam, min = 0.0001, max = 3),
      alpha = BVAR::bv_alpha(mode = 2, min = 1, max = 3),
      psi = BVAR::bv_psi(
        mode = sqrt(var_base),
        min = sqrt(var_base) / 100,
        max = sqrt(var_base) * 100)
    )
  )
  
  set.seed(54973997)
  mod_tmp <- BVAR::bvar(
    data = endogenous, 
    lags = 3, 
    exogen = exogenous,
    priors = priors_tmp,
    n_draw = 10000,
    n_burn = 1000, 
    verbose = FALSE
  )
  
  table_lambda_sensitivity <- rbind(table_lambda_sensitivity, data.frame(
    lambda_mode = lam,
    Log_Marginal_Likelihood = round(as.numeric(logLik(mod_tmp)), 2)
  ))
}

print(table_lambda_sensitivity)

# lambda_mode Log_Marginal_Likelihood
# 1          0.1                 1071.98
# 2          0.2                 1071.67
# 3          0.3                 1071.40
# 4          0.4                 1071.20
# 5          0.5                 1071.07
# 6          0.6                 1071.04
# 7          0.7                 1071.09
# 8          0.8                 1071.25
# 9          0.9                 1071.50
# 10         1.0                 1071.86
# 11         1.4                 1074.18
# 12         2.9                 1090.66
# 13         3.0                 1091.92 if i use hyper = "lambda" it solves

#prior 3 lag sensitivity ----

priors_spec_2 <- BVAR::bv_priors(
  hyper = "auto",
  mn = BVAR::bv_minnesota(
    lambda = BVAR::bv_lambda(mode = 1.3, #lambda is the overall tightness
                             #Balance how much freedom the prior gives the 
                             #model to let the data speak for itself against 
                             #how much you force it to rely on the prior
                             min = 0.0001, 
                             max = 3),
    
    alpha = BVAR::bv_alpha(mode = 2, #alpha is the lag decay controls the rate 
                           #at which the coefficients shrink toward zero as 
                           #the lag is reduced
                           min = 1, 
                           max = 3),
    
    psi = BVAR::bv_psi(#the psi evaluate the contraction using the intrinsic 
      #volatility of each variable
      mode = sqrt(var_base),
      min = sqrt(var_base) / 100,  
      max = sqrt(var_base) * 100
    )
  )
)

#the lag criterion is based on maximum likelihood
table_lags_bvar_2 <- data.frame()

for (p_i in 1:12) {
  set.seed(54973997)
  
  mod_tmp_2 <- BVAR::bvar(
    data = endogenous,
    lags = p_i,
    exogen = exogenous,
    priors = priors_spec_2,
    n_draw = 10000,
    n_burn = 1000,
    verbose = FALSE
  )
  
  #get the log-marginal likelihood
  lml_val_2 <- logLik(mod_tmp_2)
  
  table_lags_bvar_2 <- rbind(table_lags_bvar_2, data.frame(
    lag_p = p_i,
    Log_Marginal_Likelihood_2 = round(as.numeric(lml_val_2), 2)
  ))
}

print(table_lags_bvar_2)

#resuls
# lag_p Log_Marginal_Likelihood_2
# 1      1                    950.10
# 2      2                   1095.25 winner
# 3      3                   1073.47
# 4      4                   1053.77
# 5      5                   1055.44
# 6      6                   1053.56
# 7      7                   1051.11
# 8      8                   1035.85
# 9      9                   1022.75
# 10    10                   1001.73
# 11    11                    991.65
# 12    12                    967.41

#final hiperparameters ----

priors_spec_3 <- BVAR::bv_priors(
  hyper = "lambda",
  mn = BVAR::bv_minnesota(
    # Literature standard for small economies with interpolated quarterly data
    #Giannone, D., Lenza, M., & Primiceri, G. E. (2015), "Prior selection..."
    # Fixing lambda around 0.2 avoids overfitting the smoothness of Denton-Cholette
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

# bvar with p=2 ----

set.seed(54973997)
timor <- BVAR::bvar(
  data = endogenous,
  lags = 2,
  exogen = exogenous,
  priors = priors_spec_3,
  n_draw = 300000, #mcmc totals
  n_burn = 100000, #warm-up period (initial discard)
  n_thin = 50, #keep 1 out of every 50 samples, remove autocorrelation  mcmc
  verbose = TRUE #the console displays a progress bar 
)

summary(timor)
# Bayesian VAR consisting of 86 observations, 5 variables and 2 lags.
# Time spent calculating: 1.36 mins
# Hyperparameters: lambda 
# Hyperparameter values after optimisation: 2.66922
# Iterations (burnt / thinning): 300000 (100000 / 50)
# Accepted draws (rate): 199973 (1)
# 
# Numeric array (dimensions 11, 5) of coefficient values from a BVAR.
# Median values:
#   ln_gov_exp ln_gdp_non ln_imports ln_cpi ln_credit
# constant             0.139      0.180     -0.096  0.105    -0.149
# ln_gov_exp-lag1      1.517     -0.003      0.290  0.006    -0.127
# ln_gdp_non-lag1     -0.015      1.270      0.311 -0.060    -0.122
# ln_imports-lag1      0.147      0.032      1.490  0.021    -0.049
# ln_cpi-lag1          0.102      0.004      0.218  1.296     0.534
# ln_credit-lag1      -0.067     -0.023     -0.008 -0.005     1.572
# ln_gov_exp-lag2     -0.551      0.023     -0.202  0.004     0.175
# ln_gdp_non-lag2      0.060     -0.366     -0.165  0.004     0.115
# ln_imports-lag2     -0.182     -0.015     -0.617 -0.005    -0.026
# ln_cpi-lag2         -0.101      0.026     -0.327 -0.280    -0.358
# ln_credit-lag2       0.062      0.032      0.007  0.009    -0.654
# 
# Numeric array (dimensions 5, 5) of variance-covariance values from a BVAR.
# Median values:
#   ln_gov_exp ln_gdp_non ln_imports ln_cpi ln_credit
# ln_gov_exp      0.001          0      0.000      0     0.000
# ln_gdp_non      0.000          0      0.000      0     0.000
# ln_imports      0.000          0      0.002      0     0.000
# ln_cpi          0.000          0      0.000      0     0.000
# ln_credit       0.000          0      0.000      0     0.003
# 
# Log-Likelihood: 1095.308 

plot(timor)

#hiperparameters
#timor$hyper

#coefficients
#coef(timor, type = "mean")

#extract residuals
# Note on Denton-Cholette disaggregation:
# The quarterly series were interpolated from annual data without guide variables
# This introduces deterministic smoothness and intrinsic serial correlation
# Therefore, Ljung-Box test rejections for autocorrelation should be interpreted
# with caution as a property of the temporal disaggregation method, not solely 
# BVAR misspecification.

res_array <- stats::residuals(timor)
res_median <- residuals(timor, type = "quantile", conf_bands = 0.5)
res_median <- as.matrix(res_median)
colnames(res_median) <- colnames(endogenous)

#ARCH and Ljung-Box test ----
diagnostics_table <- data.frame()

for (i in 1:ncol(res_median)) {
  var_name <- colnames(res_median)[i]
  var_resid <- res_median[, i]
  
  #autocorrelation (Ljung-Box test)
  #H0: there is no autocorrelation (p > 0.05 and true)
  lb_test <- stats::Box.test(var_resid, lag = 12, type = "Ljung-Box")
  p_lb <- lb_test$p.value
  
  #heteroscedasticity (ARCH test)
  #H0: no ARCH effects(p > 0.05 and true)
  arch_test <- FinTS::ArchTest(var_resid, lags = 12)
  p_arch <- arch_test$p.value
  
  diagnostics_table <- rbind(diagnostics_table, data.frame(
    Variable = var_name,
    P_Value_Autocorr = round(p_lb, 4),
    Pass_Autocorr = p_lb > 0.05,
    P_Value_Heterosc = round(p_arch, 4),
    Pass_Heterosc = p_arch > 0.05
  ))
}

cat("Residual Diagnostics (Ljung-Box & ARCH)\n")
print(diagnostics_table)
# Variable P_Value_Autocorr Pass_Autocorr P_Value_Heterosc Pass_Heterosc
# Chi-squared  ln_gov_exp            0e+00         FALSE           0.0000         FALSE
# Chi-squared1 ln_gdp_non            0e+00         FALSE           0.0207         FALSE
# Chi-squared2 ln_imports            0e+00         FALSE           0.0000         FALSE
# Chi-squared3     ln_cpi            0e+00         FALSE           0.2346          TRUE
# Chi-squared4  ln_credit            6e-04         FALSE           0.0001         FALSE

#mcmc convergence test (geweke & ess) ----

# assessing convergence across coefficients (beta) and covariance (sigma)

#mcmc convergence, coefficients beta, ess and geweke 
beta_mat <- matrix(timor$beta, nrow = dim(timor$beta)[1])
beta_mcmc <- coda::as.mcmc(beta_mat)
ess_beta <- coda::effectiveSize(beta_mcmc)
geweke_beta <- coda::geweke.diag(beta_mcmc)
geweke_p_beta <- 2 * stats::pnorm(-abs(geweke_beta$z))

beta_table <- data.frame(
  Parameter = paste0("beta_", 1:ncol(beta_mat)),
  Geweke_P_Value = round(geweke_p_beta, 4),
  Eff_Sample_Size = round(ess_beta, 1),
  Pass_Convergence = geweke_p_beta > 0.05
)
print(beta_table)
# Parameter Geweke_P_Value Eff_Sample_Size Pass_Convergence
# var1     beta_1         0.9981          4000.0             TRUE
# var2     beta_2         0.2063          4000.0             TRUE
# var3     beta_3         0.8413          4000.0             TRUE
# var4     beta_4         0.3113          4000.0             TRUE
# var5     beta_5         0.7010          4000.0             TRUE
# var6     beta_6         0.8912          4000.0             TRUE
# var7     beta_7         0.2579          4453.1             TRUE
# var8     beta_8         0.6832          4000.0             TRUE
# var9     beta_9         0.4148          4000.0             TRUE
# var10   beta_10         0.9441          4000.0             TRUE
# var11   beta_11         0.8569          4000.0             TRUE
# var12   beta_12         0.5300          4000.0             TRUE
# var13   beta_13         0.5923          4000.0             TRUE
# var14   beta_14         0.0334          4000.0            FALSE
# var15   beta_15         0.6503          4000.0             TRUE
# var16   beta_16         0.3076          4190.4             TRUE
# var17   beta_17         0.1507          4000.0             TRUE
# var18   beta_18         0.6439          4000.0             TRUE
# var19   beta_19         0.0279          4000.0            FALSE
# var20   beta_20         0.7436          4215.8             TRUE
# var21   beta_21         0.2578          4000.0             TRUE
# var22   beta_22         0.2987          4000.0             TRUE
# var23   beta_23         0.9510          4000.0             TRUE
# var24   beta_24         0.0816          4000.0             TRUE
# var25   beta_25         0.6064          4000.0             TRUE
# var26   beta_26         0.1367          4000.0             TRUE
# var27   beta_27         0.5058          4000.0             TRUE
# var28   beta_28         0.6858          4000.0             TRUE
# var29   beta_29         0.0343          4184.3            FALSE
# var30   beta_30         0.4727          3991.5             TRUE
# var31   beta_31         0.0301          4000.0            FALSE
# var32   beta_32         0.5260          4000.0             TRUE
# var33   beta_33         0.7544          4000.0             TRUE
# var34   beta_34         0.7200          4546.6             TRUE
# var35   beta_35         0.7791          5274.7             TRUE
# var36   beta_36         0.2401          4000.0             TRUE
# var37   beta_37         0.3142          4000.0             TRUE
# var38   beta_38         0.8444          4000.0             TRUE
# var39   beta_39         0.7510          4000.0             TRUE
# var40   beta_40         0.8318          3420.4             TRUE
# var41   beta_41         0.3354          3795.0             TRUE
# var42   beta_42         0.5938          4000.0             TRUE
# var43   beta_43         0.7321          4000.0             TRUE
# var44   beta_44         0.5777          4000.0             TRUE
# var45   beta_45         0.0499          4000.0            FALSE
# var46   beta_46         0.8725          3798.6             TRUE
# var47   beta_47         0.2679          4000.0             TRUE
# var48   beta_48         0.5922          4000.0             TRUE
# var49   beta_49         0.6311          4000.0             TRUE
# var50   beta_50         0.7462          4000.0             TRUE
# var51   beta_51         0.9466          4000.0             TRUE
# var52   beta_52         0.1010          4436.6             TRUE
# var53   beta_53         0.4882          4000.0             TRUE
# var54   beta_54         0.4528          4000.0             TRUE
# var55   beta_55         0.6521          4000.0             TRUE

#mcmc convergence variance covariance (sigma)
sigma_mat <- matrix(timor$sigma, nrow = dim(timor$sigma)[1])
sigma_mcmc <- coda::as.mcmc(sigma_mat)
ess_sigma <- coda::effectiveSize(sigma_mcmc)
geweke_sigma <- coda::geweke.diag(sigma_mcmc)
geweke_p_sigma <- 2 * stats::pnorm(-abs(geweke_sigma$z))

sigma_table <- data.frame(
  Parameter = paste0("sigma_", 1:ncol(sigma_mat)),
  Geweke_P_Value = round(geweke_p_sigma, 4),
  Eff_Sample_Size = round(ess_sigma, 1),
  Pass_Convergence = geweke_p_sigma > 0.05
)
print(sigma_table)
# Parameter Geweke_P_Value Eff_Sample_Size Pass_Convergence
# var1    sigma_1         0.9038          4000.0             TRUE
# var2    sigma_2         0.3170          4000.0             TRUE
# var3    sigma_3         0.1780          4000.0             TRUE
# var4    sigma_4         0.7833          4506.5             TRUE
# var5    sigma_5         0.1928          4000.0             TRUE
# var6    sigma_6         0.3170          4000.0             TRUE
# var7    sigma_7         0.3364          4000.0             TRUE
# var8    sigma_8         0.0695          4000.0             TRUE
# var9    sigma_9         0.9421          4000.0             TRUE
# var10  sigma_10         0.4579          4000.0             TRUE
# var11  sigma_11         0.1780          4000.0             TRUE
# var12  sigma_12         0.0695          4000.0             TRUE
# var13  sigma_13         0.3354          3661.4             TRUE
# var14  sigma_14         0.3905          4000.0             TRUE
# var15  sigma_15         0.5837          3812.7             TRUE
# var16  sigma_16         0.7833          4506.5             TRUE
# var17  sigma_17         0.9421          4000.0             TRUE
# var18  sigma_18         0.3905          4000.0             TRUE
# var19  sigma_19         0.7662          4000.0             TRUE
# var20  sigma_20         0.3278          4000.0             TRUE
# var21  sigma_21         0.1928          4000.0             TRUE
# var22  sigma_22         0.4579          4000.0             TRUE
# var23  sigma_23         0.5837          3812.7             TRUE
# var24  sigma_24         0.3278          4000.0             TRUE
# var25  sigma_25         0.0017          4178.6            FALSE

#BVAR stability 
comp_mat <- BVAR::companion(timor)
eigen_vals <- eigen(comp_mat)$values
max_modulus <- max(Mod(eigen_vals))

stability_df <- data.frame(
  Max_Eigenvalue_Modulus = round(max_modulus, 4),
  Is_Stable = max_modulus < 1
)
print(stability_df)
# Max_Eigenvalue_Modulus Is_Stable
# 1                 0.9929      TRUE

#plot
df_eigen <- data.frame(
  Real = Re(eigen_vals),
  Imag = Im(eigen_vals)
)

circle_data <- data.frame(
  x = cos(seq(0, 2 * pi, length.out = 100)),
  y = sin(seq(0, 2 * pi, length.out = 100))
)

stability_plot <- ggplot2::ggplot() +
  ggplot2::geom_path(data = circle_data, ggplot2::aes(x = x, y = y), color = "black") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  ggplot2::geom_point(data = df_eigen, ggplot2::aes(x = Real, y = Imag), 
                      color = "darkred", size = 3, alpha = 0.7) +
  ggplot2::coord_fixed() +
  ggplot2::labs(title = "Inverse Roots of AR Characteristic Polynomial",
                x = "Real",
                y = "Imaginary") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                 panel.border = ggplot2::element_rect(color = "black", fill = NA))

print(stability_plot)

ggplot2::ggsave("graphics/03_bvar_stability_roots.pdf", 
                plot = stability_plot, 
                width = 6, height = 6)

# irf ----

# ident = TRUE applies cholesky orthogonalization based on variable order
irf_timor <- BVAR::irf(
  timor,
  horizon = 20, 
  ident = TRUE, 
  fevd = TRUE, #also computes forecast error variance decomposition
  conf_bands = c(0.10, 0.16, 0.84, 0.90) # Computes 68% (0.16-0.84) 
  #and 80% (0.10-0.90) intervals
)

plot(irf_timor, 
     vars_impulse = "ln_gov_exp", 
     vars_response = c("ln_imports", "ln_gdp_non", "ln_cpi", "ln_credit"))

plot(irf_timor)

