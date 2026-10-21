#question to answer

if(F){
  "
  
  how real non-oil GDP responds to public spending and what 
  portion of the fiscal stimulus is reflected in higher imports
  
  
  "
}

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
#note if the library are update, use CRAN = "https://packagemanager.posit.co/cran/2026-10-19"
#this install 2026-10-19 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(readxl,
               tidyverse,
               janitor,
               dplyr,
               tsibble
)

data_timor <- readxl::read_excel("Data/data-source/tim-key-indicators-2025.xlsx", 
                         sheet = "TIM", 
                         range = "B7:AA424", 
                         col_names = FALSE)



print(data_timor)

#cleaning the dataframe ----

#transpose
data_timor <- t(data_timor)

#the t() funcion converts it to a matrix, convert it to a dataframe
data_timor <- as.data.frame(data_timor)

#header
data_timor <- data_timor %>%
  janitor::row_to_names(row_number = 1)

#rename the first row year
colnames(data_timor)[1] <- "year"

#cleanig the names 
colnames(data_timor) <- janitor::make_clean_names(colnames(data_timor))

#dalate na columns
#note: to use matches, it need to be a dataframe
data_timor <- data_timor %>%
  dplyr::select(!dplyr::matches("^na(_\\d+)?$")) 

#convert '…' and '-' to NA 
#note: across allows apply a funcion to each column,
#everything is a filter without exception,
#\(columns) is the same that funcion(columns) 
data_timor <- data_timor %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), \(columns) dplyr::na_if(columns, "…"))) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), \(columns) dplyr::na_if(columns, "–")))

#delate the columns that all there obs are na
data_timor <- data_timor %>%
  dplyr::select(dplyr::where(\(columns) !all(is.na(columns))))

#fix row header
rownames(data_timor) <- NULL

#save timor-leste's dataset
#note: it's not completely cleaned, but it's in dataframe format

readr::write_csv(data_timor, "Data/csv/timor_leste_uncleaned.csv")

#select the variables ----

names(data_timor)

annual_data <- data_timor %>%
  dplyr::select(
  year,
  gov_expenditure_real = government_final_consumptionh_2,
  gdp_non_oil_real = gdp_by_industrial_origin_at_constant_2015_market_prices,
  imports_real = imports_of_goods_and_services_2,
  cpi  = consumer_dili_december_2001_december_2012_august_2018_100,
  private_credit = claims_on_private_sector
) %>%
  dplyr::arrange(year)

annual_data <- annual_data %>%
  tidyr::drop_na()

print(annual_data)

tibble::as_tibble(annual_data)

readr::write_csv(annual_data, "Data/csv/timor_annual_macro.csv")


# imports ----

imports_raw <- readr::read_csv("Data/data-source/timor-leste-imports-exports-to-timor-leste-by-product_all.csv") 

print(imports_raw)

#transpose
imports <- t(imports_raw)

#the t() funcion converts it to a matrix, convert it to a dataframe
imports <- as.data.frame(imports)

#row without header
imports <- imports %>%
  tibble::rownames_to_column(var = "na")

#header
imports <- imports %>%
  janitor::row_to_names(row_number = 6)

#rename the first row year
colnames(imports)[1] <- "year"

#cleanig the names 
colnames(imports) <- janitor::make_clean_names(colnames(imports))

tibble::as_tibble(imports)

print(imports$year)

imports %>%
  dplyr::select(year) %>%
  print()

#date claning

imports <- imports %>%
  dplyr::mutate(
    #extract only the first 6 characters (200201)
    year_raw = substr(year, 1, 6),
    
    #separate the year (first 4 digits) and the quarter (last 2 digits)
    yyyy = substr(year_raw, 1, 4),
    qq = substr(year_raw, 5, 6),
  ) %>% 
  dplyr::mutate(
    #assign the starting month for each quarter
    month = dplyr::case_when(
      qq == "01" ~ "01", #quarter 1 begins in january
      qq == "02" ~ "04", #quarter 2 begins in april
      qq == "03" ~ "07", #quarter 3 begins in july
      qq == "04" ~ "10"  #quarter 4 begins in october
    ),
    day = "-01",
    year = as.Date(paste0(yyyy, "-", month, day))
  )

imports %>% 
  dplyr::select(year) %>% 
  print()

colnames(imports)[1] <- "date"
colnames(imports)[2] <- "imports"

#imports in millons of dollars
imports <- imports %>%
  dplyr::mutate(
    imports = as.numeric(imports) * 1000,
    imports = as.numeric(imports) / 1000000
  )

imports <- imports %>%
  dplyr::select(date, imports) %>%
  tibble::as_tibble() %>%
  print(n = 88)

#outlier at 2010-10-01 
#628 is too high, it migth be a typing error

imports$imports[36] <- imports$imports[36] / 10

imports %>%
  dplyr::select(date, imports) %>%
  tibble::as_tibble() %>%
  print(n = 88)

readr::write_csv(imports, "Data/csv/imports_cleaning_timor_leste.csv")

#cpi ----

cpi <- readxl::read_excel("Data/data-source/CPI Publish Readj New Weight.xls", 
                  sheet = "More", range = "A10:O90", col_names = FALSE)

cpi_hola <- cpi %>%
  dplyr::filter(!...1 %in% c("Monthly", "Quarterly", "Year on Year"))

cpi_hola <- cpi_hola %>%
  dplyr::filter(...1 %in% "Index")

dplyr::glimpse(cpi_hola)

cpi_matrix <- cpi_hola %>%
  dplyr::select(...3:...14) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
  as.matrix()

cpi_vector <- as.vector(t(cpi_matrix))

cpi_ts <- stats::ts(cpi_vector, start = c(2013, 1), frequency = 12)

cpi_ts <- as_tsibble(cpi_ts)

dplyr::glimpse(cpi_ts)

cpi_months <- cpi_ts %>%
  tibble::as_tibble() %>%
  dplyr::mutate(date = as.Date(index)) %>%
  dplyr::select(date, cpi_months = value)

readr::write_csv(cpi_months,"Data/csv/cpi_months.csv")



