rm(list = ls())
#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               tempdisagg,
               zoo,
               timeSeries
               )

# import data if it doesn't exist in the environment


annual_data <- readr::read_csv("Data/csv/timor_annual_macro.csv")
imports <- readr::read_csv("Data/csv/imports_cleaning_timor_leste.csv")
cpi_raw <- readr::read_csv("Data/csv/cpi_months.csv")

#time-series ----

start_year <- min(annual_data$year)

# gov_exp <- ts(annual_data$gov_expenditure_real, 
#                  start = start_year, 
#                  frequency = 1)
# 
# gdp_non <- ts(annual_data$gdp_non_oil_real,
#                  start = 2002, 
#                  frequency = 1)
# 
# imports <- ts(annual_data$imports_real,
#                  start = 2002,
#                  frequency = 1)
# 
# cpi <- ts(annual_data$cpi,
#                  start = 2002, 
#                  frequency = 1)
# 
# credit <- ts(annual_data$private_credit,
#                  start = 2002, 
#                  frequency = 1)

var_names <- c(
  "gov_expenditure_real", 
  "gdp_non_oil_real", 
  "imports_real", 
  "cpi", 
  "private_credit"
)

#note: assign creates string
for (v in var_names) {
  assign(v, 
         stats::ts(annual_data[[v]], 
            start = start_year, 
            frequency = 1))
}

dplyr::glimpse(cpi_raw)

cpi_months_timor <- stats::ts(cpi_raw$cpi_months,
                 start = 2013,
                 frequency = 12)


#interporlate imports ----

imports <- imports %>%
  dplyr::mutate(
    imports = zoo::na.approx(imports, rule = 2) # 2 (2002-04-01   0.002) 
    #extrapolate the first value backward
  )

head(imports, n = 10)

ts_imports_q <- ts(imports$imports, start = c(2002, 1), frequency = 4)

#end timeseries
ts_imports_q <- window(ts_imports_q, end = c(2023, 4))


#make the time-series in to quarterly ----
#last year is 2023

#note: ~1 indicates that it has no external quarterly indicators
#method is the type of smoothing algorithm
#conversion = sum fallow this form 
#$$\sum_{q=1}^{4} x_{t,q} = X_t^{anual}$$
#to average
#$$\frac{1}{4} \sum_{q=1}^{4} x_{t,q} = X_t^{anual}$$


# q_gdp_non_model <- tempdisagg::td(gdp_non_oil_real ~ ts_imports_q, 
#                                     method = "chow-lin-maxlog", 
#                                     conversion = "sum")
# 
# q_gdp_non <- stats::predict(q_gdp_non_model)
# 
# summary(q_gdp_non_model)

# Call:
#   tempdisagg::td(formula = gdp_non_oil_real ~ ts_imports_q, conversion = "sum", 
#                  method = "chow-lin-maxlog")
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -301.2 -187.5  127.1  221.9  324.4 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  288.6893    48.2720    5.98 7.56e-06 ***
#   ts_imports_q   0.3298     0.1736    1.90    0.072 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 'chow-lin-maxlog' disaggregation with 'sum' conversion
# 22 low-freq. obs. converted to 88 high-freq. obs.
# Adjusted R-squared: 0.1105	AR1-Parameter: 0.9848

# no significant

# q_imports_model <- tempdisagg::td(imports_real ~ ts_imports_q, 
#                                     method = "chow-lin-maxlog", 
#                                     conversion = "sum")
# 
# q_imports <- stats::predict(q_imports_model)
# 
# summary(q_imports_model)

# Call:
#   tempdisagg::td(formula = imports_real ~ ts_imports_q, conversion = "sum", 
#                  method = "chow-lin-maxlog")
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -460.94 -135.09  -55.90   26.27  378.24 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  217.7737    38.2836   5.688 1.44e-05 ***
#   ts_imports_q   0.1706     0.2959   0.576    0.571    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 'chow-lin-maxlog' disaggregation with 'sum' conversion
# 22 low-freq. obs. converted to 88 high-freq. obs.
# Adjusted R-squared: -0.03284	AR1-Parameter: 0.8874

#its better not tos use high frequency data

q_gdp_non_model <- tempdisagg::td(
  gdp_non_oil_real ~ 1,
  to = "quarterly",
  method = "denton-cholette",
  criterion = "proportional",
  conversion = "sum"
)

q_gdp_non <- stats::predict(q_gdp_non_model)

summary(q_gdp_non_model)

q_gov_exp <- stats::predict(
  tempdisagg::td(gov_expenditure_real ~ 1, 
                 to = "quarterly", 
                 method = "denton-cholette", 
                 criterion = "proportional", 
                 conversion = "sum"))

q_imports <- stats::predict(
  tempdisagg::td(imports_real ~ 1, 
                 to = "quarterly", 
                 method = "denton-cholette", 
                 criterion = "proportional", 
                 conversion = "sum")
)

q_cpi_model <- tempdisagg::td(cpi ~ 1, 
                              to = "quarterly", 
                              method = "denton-cholette", 
                              criterion = "proportional", 
                              conversion = "average"
                              )
summary(q_cpi_model)

q_cpi <- stats::predict(q_cpi_model)

q_observed_cpi <- stats::aggregate(cpi_months_timor, nfrequency = 4, FUN = mean)

stats::window(q_cpi, start = c(2013, 1), end = end(q_observed_cpi)) <- q_observed_cpi

plot(q_cpi, main = "Timor-Leste: CPI Quarterly (2002-2025)", ylab = "Index", xlab = "Time")
graphics::abline(v = 2013, col = "red", lty = 2)

q_credit <- stats::predict(
  tempdisagg::td(private_credit ~ 1,
                 to = "quarterly", 
                 method = "denton-cholette", 
                 criterion = "proportional", 
                 conversion = "average"
  ))

#quarter dataset ----

# quarterly_data <- tibble::tibble(
#   date = seq(from = as.Date(paste0(start_year, "-01-01")), 
#              by= "quarter", 
#              #the length of dataframe
#              length.out = length(q_gdp_non)
#   ),
#   gov_expenditure_real = as.numeric(q_gov_exp),
#   gdp_non_oil_real = as.numeric(q_gdp_non),
#   imports_real = as.numeric(q_imports),
#   cpi = as.numeric(q_cpi),
#   private_credit = as.numeric(q_credit)
# )

ts_matrix <- stats::ts.intersect(
  gov_expenditure_real = q_gov_exp,
  gdp_non_oil_real = q_gdp_non,
  imports_real = q_imports,
  cpi = q_cpi,
  private_credit = q_credit
)

quarterly_data <- tibble::as_tibble(ts_matrix) %>%
  tibble::add_column(
    date = seq(
      from = as.Date(paste0(start_year, "-01-01")),
      by = "quarter",
      length.out = nrow(ts_matrix)
    ),
    .before = 1
  )

dplyr::glimpse(quarterly_data)

#log dataset

quarterly_log <- quarterly_data %>%
  #also it can use transmute, delate everything less the new variables
  dplyr::mutate(
    ln_gov_exp = log(gov_expenditure_real),
    ln_gdp_non = log(gdp_non_oil_real),
    ln_imports = log(imports_real),
    ln_cpi = log(cpi),
    ln_credit = log(private_credit)
  )

quarterly_log <- quarterly_log %>%
  dplyr::select(
    date, dplyr::starts_with("ln_")
  )

tibble::as_tibble(quarterly_data)
tibble::as_tibble(quarterly_log)


#export to csv
readr::write_csv(quarterly_data, "Data/csv/timor_quarterly_data.csv")
readr::write_csv(quarterly_log,  "Data/csv/timor_quarterly_log.csv")