rm(list = ls())
#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               ggplot2
)

# import data if it doesn't exist in the environment

quarterly_log_dummy <- readr::read_csv("Data/csv/timor_quarterly_log_and_dummys.csv")
timor_quarterly_data <- readr::read_csv("Data/csv/timor_quarterly_data.csv")
 

#graphics config ----

df_levels <- timor_quarterly_data %>%
  dplyr::select(date, gov_expenditure_real, gdp_non_oil_real, imports_real) %>%
  tidyr::pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    variable = dplyr::case_when(
      variable == "gdp_non_oil_real" ~ "Non-Oil GDP",
      variable == "gov_expenditure_real" ~ "Government Expenditure",
      variable == "imports_real" ~ "Imports"
    )
  )

df_logs <- quarterly_log_dummy %>%
  dplyr::select(date, ln_gov_exp, ln_gdp_non, ln_imports) %>%
  tidyr::pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    variable = dplyr::case_when(
      variable == "ln_gdp_non" ~ "ln(Non-Oil GDP)",
      variable == "ln_gov_exp" ~ "ln(Government Expenditure)",
      variable == "ln_imports" ~ "ln(Imports)"
    )
  )

#exogenous shock 
shocks <- data.frame(
  xmin = as.Date(c("2005-10-01", "2009-01-01", "2012-10-01", "2016-01-01", "2020-01-01")),
  xmax = as.Date(c("2007-09-30", "2010-12-31", "2014-09-30", "2018-12-31", "2021-12-31")),
  ymin = -Inf, ymax = Inf
)

#bic breakpoint (Bai & Perron)\
breaks_bp <- as.Date(c("2005-10-01", "2009-04-01", "2012-10-01", "2016-07-01", "2020-07-01"))

#colors
color_palette <- c("Government Expenditure" = "#b30000", 
                   "Imports" = "#4d4d4d", 
                   "Non-Oil GDP" = "#004c99",
                   "ln(Government Expenditure)" = "#b30000", 
                   "ln(Imports)" = "#4d4d4d", 
                   "ln(Non-Oil GDP)" = "#004c99")

#plots ----

#plot 1, levels
p1 <- ggplot2::ggplot() +
  ggplot2::geom_rect(data = shocks, 
                     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                     fill = "gray85", alpha = 0.6, inherit.aes = FALSE) +
  ggplot2::geom_line(data = df_levels, 
                     aes(x = date, y = value, color = variable), 
                     linewidth = 1.1) +   
  ggplot2::scale_color_manual(values = color_palette) +
  ggplot2::labs(
    # title = "Macroeconomic Dynamics of Timor-Leste (2002-2023)",
    # subtitle = "Government spending, non-oil GDP, and imports with exogenous structural shocks (shaded)",
    x = "",
    y = "Millions USD (Constant 2015 Prices)",
    color = ""
  ) +
  ggplot2::theme_classic(base_family = "serif", base_size = 14) +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold", size = 16),
    panel.grid.major.y = ggplot2::element_line(color = "gray90")
  )

#plot 2, log
p2 <- ggplot2::ggplot() +
  ggplot2::geom_vline(xintercept = breaks_bp, linetype = "dotted", color = "black", linewidth = 0.9) +
  ggplot2::geom_line(data = df_logs, 
                     aes(x = date, y = value, color = variable), 
                     linewidth = 1.1) +   
  ggplot2::scale_color_manual(values = color_palette) +
  ggplot2::labs(
    # title = "Logarithmic Transformation and Structural Breakpoints",
    # subtitle = "Vertical dotted lines indicate optimal regime partitions under the BIC criterion (Bai & Perron)",
    x = "Year",
    y = "Logarithmic Scale",
    color = ""
  ) +
  ggplot2::theme_classic(base_family = "serif", base_size = 14) +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold", size = 16),
    panel.grid.major.y = ggplot2::element_line(color = "gray90")
  )

grDevices::pdf("graphics/figure_1_macro_levels.pdf", width = 11, height = 6.5, family = "serif")
base::print(p1)
grDevices::dev.off()

grDevices::pdf("graphics/figure_2_macro_log.pdf", width = 11, height = 6.5, family = "serif")
base::print(p2)
grDevices::dev.off()

