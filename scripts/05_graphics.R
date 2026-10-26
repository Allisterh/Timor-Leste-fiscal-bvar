rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, ggplot2)

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

official_levels <- readr::read_csv(
  "Data/csv/timor_quarterly_official.csv",
  show_col_types = FALSE
) %>%
  dplyr::mutate(date = as.Date(date))

official_logs <- readr::read_csv(
  "Data/csv/timor_quarterly_official_log_and_dummys.csv",
  show_col_types = FALSE
) %>%
  dplyr::mutate(date = as.Date(date))

long_levels <- readr::read_csv(
  "Data/csv/timor_quarterly_data.csv",
  show_col_types = FALSE
) %>%
  dplyr::mutate(date = as.Date(date))

long_logs <- readr::read_csv(
  "Data/csv/timor_quarterly_log_and_dummys.csv",
  show_col_types = FALSE
) %>%
  dplyr::mutate(date = as.Date(date))

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

color_palette <- c(
  "Government Expenditure" = "#b30000",
  "Imports" = "#4d4d4d",
  "Non-Oil GDP" = "#004c99",
  "ln(Government Expenditure)" = "#b30000",
  "ln(Imports)" = "#4d4d4d",
  "ln(Non-Oil GDP)" = "#004c99"
)

make_levels <- function(data) {
  data %>%
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
}

make_logs <- function(data) {
  data %>%
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
}

base_theme <- ggplot2::theme_classic(base_family = "serif", base_size = 14) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.major.y = ggplot2::element_line(color = "gray90")
  )

# ---------------------------------------------------------------------------
# Official sample figures
# ---------------------------------------------------------------------------

official_shocks <- data.frame(
  xmin = as.Date(c("2016-01-01", "2020-01-01")),
  xmax = as.Date(c("2018-12-31", "2021-12-31")),
  ymin = -Inf,
  ymax = Inf
)

p1_official <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = official_shocks,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "gray85", alpha = 0.6, inherit.aes = FALSE
  ) +
  ggplot2::geom_line(
    data = make_levels(official_levels),
    ggplot2::aes(x = date, y = value, color = variable),
    linewidth = 1.1
  ) +
  ggplot2::scale_color_manual(values = color_palette) +
  ggplot2::labs(
    x = "",
    y = "Millions USD (constant 2015 prices)",
    color = ""
  ) +
  base_theme

p2_official <- ggplot2::ggplot() +
  ggplot2::geom_vline(
    xintercept = as.Date(c("2016-01-01", "2020-01-01")),
    linetype = "dotted", color = "black", linewidth = 0.9
  ) +
  ggplot2::geom_line(
    data = make_logs(official_logs),
    ggplot2::aes(x = date, y = value, color = variable),
    linewidth = 1.1
  ) +
  ggplot2::scale_color_manual(values = color_palette) +
  ggplot2::labs(
    x = "Year",
    y = "Logarithmic scale",
    color = ""
  ) +
  base_theme

ggplot2::ggsave(
  "graphics/figure_1_macro_levels_official.pdf",
  plot = p1_official, width = 11, height = 6.5
)
ggplot2::ggsave(
  "graphics/figure_2_macro_log_official.pdf",
  plot = p2_official, width = 11, height = 6.5
)

# ---------------------------------------------------------------------------
# Long-sample figures
# ---------------------------------------------------------------------------

long_shocks <- data.frame(
  xmin = as.Date(c(
    "2005-10-01", "2009-01-01", "2012-10-01", "2016-01-01", "2020-01-01"
  )),
  xmax = as.Date(c(
    "2007-09-30", "2010-12-31", "2014-09-30", "2018-12-31", "2021-12-31"
  )),
  ymin = -Inf,
  ymax = Inf
)

long_breaks <- as.Date(c(
  "2005-10-01", "2009-04-01", "2012-10-01", "2016-07-01", "2020-07-01"
))

p1_long <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = long_shocks,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "gray85", alpha = 0.6, inherit.aes = FALSE
  ) +
  ggplot2::geom_line(
    data = make_levels(long_levels),
    ggplot2::aes(x = date, y = value, color = variable),
    linewidth = 1.1
  ) +
  ggplot2::scale_color_manual(values = color_palette) +
  ggplot2::labs(
    x = "",
    y = "Millions USD (constant 2015 prices)",
    color = ""
  ) +
  base_theme

p2_long <- ggplot2::ggplot() +
  ggplot2::geom_vline(
    xintercept = long_breaks,
    linetype = "dotted", color = "black", linewidth = 0.9
  ) +
  ggplot2::geom_line(
    data = make_logs(long_logs),
    ggplot2::aes(x = date, y = value, color = variable),
    linewidth = 1.1
  ) +
  ggplot2::scale_color_manual(values = color_palette) +
  ggplot2::labs(
    x = "Year",
    y = "Logarithmic scale",
    color = ""
  ) +
  base_theme

ggplot2::ggsave(
  "graphics/figure_1_macro_levels_long.pdf",
  plot = p1_long, width = 11, height = 6.5
)
ggplot2::ggsave(
  "graphics/figure_2_macro_log_long.pdf",
  plot = p2_long, width = 11, height = 6.5
)
