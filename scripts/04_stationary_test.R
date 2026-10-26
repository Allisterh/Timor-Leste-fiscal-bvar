rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, tseries, urca)

run_tests <- function(file, sample_name, suffix) {
  data <- readr::read_csv(file, show_col_types = FALSE) %>%
    dplyr::mutate(date = as.Date(date))

  variables <- data %>%
    dplyr::select(dplyr::starts_with("ln_")) %>%
    names()

  adf_kpss <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    purrr::map_dfr(\(x) {
      tibble::tibble(
        p_val_adf = tryCatch(
          round(tseries::adf.test(x)$p.value, 4),
          error = function(e) NA_real_
        ),
        p_val_kpss = tryCatch(
          suppressWarnings(round(tseries::kpss.test(x)$p.value, 4)),
          error = function(e) NA_real_
        )
      )
    }, .id = "variable")

  za <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    purrr::map_dfr(\(x) {
      fit <- tryCatch(
        urca::ur.za(x, model = "both", lag = 2),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        return(tibble::tibble(
          t_stat = NA_real_,
          crit_val_5pct = NA_real_,
          break_point = NA_integer_,
          break_date = NA_character_,
          is_stationary = NA
        ))
      }
      tibble::tibble(
        t_stat = round(fit@teststat, 4),
        crit_val_5pct = fit@cval[2],
        break_point = fit@bpoint,
        break_date = as.character(data$date[fit@bpoint]),
        is_stationary = fit@teststat < fit@cval[2]
      )
    }, .id = "variable")

  out <- adf_kpss %>%
    dplyr::left_join(za, by = "variable") %>%
    dplyr::mutate(sample = sample_name, .before = 1)

  readr::write_csv(
    out,
    paste0("Data/csv/stationarity_", suffix, ".csv")
  )

  message("\nStationarity tests: ", sample_name)
  print(out)
  invisible(out)
}

run_tests(
  "Data/csv/timor_quarterly_official_log_and_dummys.csv",
  "Official 2014Q1-2023Q4",
  "official"
)

run_tests(
  "Data/csv/timor_quarterly_log_and_dummys.csv",
  "Long 2002Q1-2023Q4",
  "long"
)
