rm(list = ls())

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               tseries,
               urca
)

# import data if it doesn't exist in the environment


quarterly_log_dummy <- readr::read_csv("Data/csv/timor_quarterly_log_and_dummys.csv")


#stationary test ----

#notes: map_dfr, it takes each column of the table and applies a 
#function to it, and stacks the results row by row
#.id create a new column
unit_root_pvalues <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("ln_")) %>%
  purrr::map_dfr(\(x) {
    tibble::tibble(
      p_val_adf = round(tseries::adf.test(x)$p.value, 4),
      p_val_kpss = round(tseries::kpss.test(x)$p.value, 4)
    )
  }, .id = "variables")

message("adf test h0: the series is non-stacionary\nkpss test h0: the series is stacionary")
print(unit_root_pvalues)
# variables  p_val_adf p_val_kpss
# <chr>          <dbl>      <dbl>
#   1 ln_gov_exp    0.0854     0.0161
# 2 ln_gdp_non    0.943      0.01  
# 3 ln_imports    0.490      0.1   
# 4 ln_cpi        0.599      0.01  
# 5 ln_credit     0.01       0.01  

#za test ----

za_results <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("ln_")) %>%
  purrr::map_dfr(\(x) {
    za_test <- urca::ur.za(x, model = "both", lag = 2)
    tibble::tibble(
      t_stat = round(za_test@teststat, 4),
      crit_val_5pct = za_test@cval[2], 
      break_point = za_test@bpoint,
      break_date = as.character(quarterly_log_dummy$date[za_test@bpoint]),
      is_stationary = za_test@teststat < za_test@cval[2]
    )
  }, .id = "variables")

print(za_results)

# variables  t_stat crit_val_5pct break_point break_date is_stationary
# <chr>       <dbl>         <dbl>       <int> <chr>      <lgl>        
#   1 ln_gov_exp  -5.09         -5.08          13 2005-01-01 TRUE         
# 2 ln_gdp_non  -3.37         -5.08          22 2007-04-01 FALSE        
# 3 ln_imports  -4.34         -5.08          25 2008-01-01 FALSE        
# 4 ln_cpi      -4.02         -5.08          32 2009-10-01 FALSE        
# 5 ln_credit  -12.7          -5.08           5 2003-01-01 TRUE      

message("is_stationary meaning that h0 is rejacted, h1 is accepted ")
message("za - h0: the series has unit root")


