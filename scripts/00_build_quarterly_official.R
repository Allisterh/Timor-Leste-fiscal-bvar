rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  jsonlite,
  lubridate,
  seasonal,
  forecast,
  zoo
)

dir.create("Data/data-source", showWarnings = FALSE)
dir.create("Data/csv", showWarnings = FALSE)

#official quarterly national accounts (INETL, 2004-2023, Table 7.1)

pdf_path <- "Data/data-source/NatlAccts2023en.pdf"
txt_path <- "Data/data-source/NatlAccts2023en.txt"
pdf_url <- "https://www.laohamutuk.org/DVD/DGS/NatlAccts2023en.pdf"

if (!file.exists(pdf_path)) {
  utils::download.file(pdf_url, pdf_path, mode = "wb", quiet = TRUE)
}

if (!file.exists(txt_path)) {
  status <- system2(
    "pdftotext",
    args = c("-layout", shQuote(pdf_path), shQuote(txt_path))
  )
  if (!identical(status, 0L)) {
    stop("pdftotext failed. Install poppler-utils and try again.")
  }
}

lines <- readLines(txt_path, warn = FALSE)

start <- grep(
  "^Table 7\\.1: Non-Oil Quarterly Gross Domestic Product by Expenditures",
  lines
)
end <- grep("^Table 7\\.2:", lines)

if (length(start) == 0 || length(end) == 0) {
  stop("Table 7.1 or Table 7.2 not found in the PDF text.")
}

block <- lines[start[1]:end[1]]

extract_row <- function(label) {
  idx <- grep(label, block, fixed = TRUE)
  if (length(idx) == 0) stop(paste("Row not found:", label))

  values <- unlist(lapply(idx, function(i) {
    tokens <- regmatches(
      block[i],
      gregexpr("-?[0-9]+(\\.[0-9]+)?", block[i])
    )[[1]]
    as.numeric(tokens)
  }))

  if (length(values) != 40) {
    stop(paste0(
      "Expected 40 quarterly values for ", label,
      " but found ", length(values), "."
    ))
  }
  values
}

gov_exp <- extract_row("General Government FCE")
imports <- extract_row("Imports of goods and services")
gdp_non <- extract_row("GDP (Expenditure )")
private_consumption <- extract_row("Private consumption")
private_investment <- extract_row("Private investment")
public_investment <- extract_row("Public investment")
development_partners <- extract_row("Development Partners")

dates <- seq(as.Date("2014-01-01"), by = "3 months", length.out = 40)

na_quarterly <- tibble::tibble(
  date = dates,
  gov_expenditure_real = gov_exp,
  gdp_non_oil_real = gdp_non,
  imports_real = imports,
  private_consumption_real = private_consumption,
  private_investment_real = private_investment,
  public_investment_real = public_investment,
  development_partners_real = development_partners,
  private_demand_real = private_consumption + private_investment,
  gdp_ex_gov_real = gdp_non - gov_exp,
  gov_broad_real = gov_exp + public_investment,
  gov_broad_dp_real = gov_exp + public_investment + development_partners
)

#CPI (monthly -> quarterly mean)

cpi_monthly <- readr::read_csv("Data/csv/cpi_months.csv", show_col_types = FALSE) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::filter(dplyr::between(date, as.Date("2014-01-01"), as.Date("2023-12-31")))

cpi_quarterly <- cpi_monthly %>%
  dplyr::mutate(q = zoo::as.yearqtr(date)) %>%
  dplyr::group_by(q) %>%
  dplyr::summarise(cpi = mean(cpi_months, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(date = as.Date(q)) %>%
  dplyr::select(date, cpi)

#credit to the private sector (BCTL monetary survey, monthly -> Q4)

credit_cache <- "Data/csv/bctl_domestic_credit_monthly.csv"

credit_monthly <- tryCatch({
  url <- paste0(
    "https://www.bancocentral.tl/stats/api/m_surveyassets/chart/",
    "?start_year=2014&end_year=2023"
  )
  raw <- jsonlite::fromJSON(url)
  tibble::tibble(
    date = lubridate::dmy(paste0("01-", raw$categories), locale = "C"),
    private_credit = as.numeric(raw$domestic_credit)
  ) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::distinct(date, .keep_all = TRUE)
}, error = function(e) {
  if (file.exists(credit_cache)) {
    readr::read_csv(credit_cache, show_col_types = FALSE) %>%
      dplyr::mutate(date = as.Date(date))
  } else {
    stop(
      "Could not download BCTL credit data and no cached file exists: ",
      conditionMessage(e)
    )
  }
})

readr::write_csv(credit_monthly, credit_cache)

credit_quarterly <- credit_monthly %>%
  dplyr::filter(lubridate::month(date) %in% c(3, 6, 9, 12)) %>%
  dplyr::mutate(q = zoo::as.yearqtr(date)) %>%
  dplyr::group_by(q) %>%
  dplyr::slice_max(date, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(date = as.Date(q)) %>%
  dplyr::select(date, private_credit)


#merge and validate

official_levels <- na_quarterly %>%
  dplyr::left_join(cpi_quarterly, by = "date") %>%
  dplyr::left_join(credit_quarterly, by = "date") %>%
  dplyr::arrange(date)

if (anyNA(official_levels)) {
  stop("The official quarterly dataset contains missing values.")
}

annual <- readr::read_csv(
  "Data/csv/timor_annual_macro.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(year >= 2014, year <= 2023)

validation <- official_levels %>%
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    gdp_q_sum = sum(gdp_non_oil_real),
    imports_q_sum = sum(imports_real),
    gov_q_sum = sum(gov_expenditure_real),
    credit_q4 = dplyr::last(private_credit),
    .groups = "drop"
  ) %>%
  dplyr::left_join(annual, by = "year") %>%
  dplyr::transmute(
    year,
    gdp_quarterly_sum = gdp_q_sum,
    gdp_annual = gdp_non_oil_real,
    gdp_diff = gdp_q_sum - gdp_non_oil_real,
    imports_quarterly_sum = imports_q_sum,
    imports_annual = imports_real,
    imports_diff = imports_q_sum - imports_real,
    gov_quarterly_sum = gov_q_sum,
    gov_annual_adb = gov_expenditure_real,
    gov_diff = gov_q_sum - gov_expenditure_real,
    credit_q4,
    credit_annual_adb = private_credit,
    credit_diff = credit_q4 - private_credit
  )

readr::write_csv(
  validation,
  "Data/csv/official_data_validation.csv"
)

gov_decomposition <- validation %>%
  dplyr::transmute(
    year,
    official_general_government_fce = gov_quarterly_sum,
    development_and_ta_on_budget = gov_annual_adb - gov_quarterly_sum,
    broad_government_expenditure_adb = gov_annual_adb
  )

readr::write_csv(
  gov_decomposition,
  "Data/csv/gov_fce_decomposition.csv"
)

readr::write_csv(
  official_levels,
  "Data/csv/timor_quarterly_official.csv"
)

accounting_decomposition <- official_levels %>%
  dplyr::transmute(
    date,
    gdp_non_oil_real,
    gov_expenditure_real,
    imports_real,
    private_consumption_real,
    private_investment_real,
    public_investment_real,
    development_partners_real,
    private_demand_real,
    gdp_ex_gov_real,
    gov_broad_real,
    gov_broad_dp_real,
    share_gov = gov_expenditure_real / gdp_non_oil_real,
    share_imports = imports_real / gdp_non_oil_real,
    share_private_demand = private_demand_real / gdp_non_oil_real,
    share_public_investment = public_investment_real / gdp_non_oil_real,
    share_development_partners = development_partners_real / gdp_non_oil_real
  )

readr::write_csv(
  accounting_decomposition,
  "Data/csv/accounting_decomposition.csv"
)

accounting_summary <- accounting_decomposition %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    mean_share_gov = mean(share_gov),
    min_share_gov = min(share_gov),
    max_share_gov = max(share_gov),
    mean_share_imports = mean(share_imports),
    min_share_imports = min(share_imports),
    max_share_imports = max(share_imports),
    mean_share_private_demand = mean(share_private_demand),
    mean_share_public_investment = mean(share_public_investment),
    mean_share_development_partners = mean(share_development_partners)
  )

readr::write_csv(
  accounting_summary,
  "Data/csv/accounting_summary.csv"
)


#logs, seasonal adjustment, and event dummies


log_levels <- official_levels %>%
  dplyr::transmute(
    date,
    ln_gov_exp = log(gov_expenditure_real),
    ln_gdp_non = log(gdp_non_oil_real),
    ln_imports = log(imports_real),
    ln_private_demand = log(private_demand_real),
    ln_gdp_ex_gov = log(gdp_ex_gov_real),
    ln_gov_broad = log(gov_broad_real),
    ln_gov_broad_dp = log(gov_broad_dp_real),
    ln_cpi = log(cpi),
    ln_credit = log(private_credit)
  )

sa_series <- function(x, name) {
  ts_x <- stats::ts(x, start = c(2014, 1), frequency = 4)

  adjusted <- tryCatch({
    fit <- seasonal::seas(ts_x)
    as.numeric(seasonal::final(fit))
  }, error = function(e) {
    message("X-13 failed for ", name, "; using forecast::seasadj fallback.")
    as.numeric(forecast::seasadj(forecast::auto.arima(ts_x, seasonal = TRUE)))
  })

  if (length(adjusted) != length(x) || anyNA(adjusted)) {
    stop("Seasonal adjustment failed for ", name)
  }
  adjusted
}

log_sa <- log_levels %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("ln_"),
      ~ sa_series(.x, dplyr::cur_column()),
      .names = "{.col}"
    )
  ) %>%
  dplyr::mutate(
    shock_2016 = dplyr::if_else(
      dplyr::between(date, as.Date("2016-01-01"), as.Date("2018-12-31")),
      1, 0
    ),
    shock_covid = dplyr::if_else(
      dplyr::between(date, as.Date("2020-01-01"), as.Date("2021-12-31")),
      1, 0
    )
  )

readr::write_csv(
  log_sa,
  "Data/csv/timor_quarterly_official_log_and_dummys.csv"
)

# NSA version with quarterly dummies, used as a robustness check.
log_nsa <- log_levels %>%
  dplyr::mutate(
    q2 = dplyr::if_else(lubridate::quarter(date) == 2, 1, 0),
    q3 = dplyr::if_else(lubridate::quarter(date) == 3, 1, 0),
    q4 = dplyr::if_else(lubridate::quarter(date) == 4, 1, 0),
    shock_2016 = dplyr::if_else(
      dplyr::between(date, as.Date("2016-01-01"), as.Date("2018-12-31")),
      1, 0
    ),
    shock_covid = dplyr::if_else(
      dplyr::between(date, as.Date("2020-01-01"), as.Date("2021-12-31")),
      1, 0
    )
  )

readr::write_csv(
  log_nsa,
  "Data/csv/timor_quarterly_official_nsa_log_and_dummys.csv"
)

print(validation)
