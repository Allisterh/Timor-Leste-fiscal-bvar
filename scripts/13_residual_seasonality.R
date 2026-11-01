rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse)

data <- readr::read_csv(
  "Data/csv/timor_quarterly_official_log_and_dummys.csv",
  show_col_types = FALSE
) %>%
  dplyr::mutate(quarter = factor(lubridate::quarter(date)))

variables <- data %>%
  dplyr::select(dplyr::starts_with("ln_")) %>%
  names()

results <- purrr::map_dfr(variables, function(v) {
  fit <- stats::lm(stats::reformulate("quarter", response = v), data = data)
  a <- stats::anova(fit)
  tibble::tibble(
    variable = v,
    f_stat = round(a$`F value`[1], 4),
    p_value = round(a$`Pr(>F)`[1], 4)
  )
})

readr::write_csv(results, "Data/csv/residual_seasonality.csv")
print(results)
