rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg) == 1) {
  dirname(normalizePath(sub("^--file=", "", script_arg)))
} else {
  normalizePath(getwd())
}
repo_root <- normalizePath(file.path(script_dir, ".."))
data_dir <- file.path(repo_root, "Data", "csv")
out_dir <- file.path(repo_root, "dashboard")

bvar_samples <- tibble::tribble(
  ~sample,        ~sample_label,
  "official_3var", "Official 2014-2023 (3-variable)",
  "long_3var",     "Long 2002-2023 (3-variable)"
)

read_bvar_raw <- function(sample, order, type) {
  prefix <- if (type == "standard") "full" else "cumulative"
  path <- file.path(
    data_dir,
    sprintf("irf_%s_order_%d_%s.csv", prefix, order, sample)
  )
  readr::read_csv(path, show_col_types = FALSE)
}

scale_for <- function(sample, order) {
  raw <- read_bvar_raw(sample, order, "standard")
  gov_impact <- raw %>%
    dplyr::filter(Impulse == "Gov Exp", Response == "Gov Exp", Horizon == 1) %>%
    dplyr::pull(`50%`)

  if (length(gov_impact) != 1 || !is.finite(gov_impact) || gov_impact <= 0) {
    stop("Could not recover a valid government expenditure impact for ", sample, ", order ", order, ".")
  }

  0.01 / gov_impact
}

bvar_data <- purrr::pmap_dfr(
  bvar_samples,
  function(sample, sample_label) {
    purrr::map_dfr(c(1, 2), function(order) {
      scale <- scale_for(sample, order)

      purrr::map_dfr(c("standard", "cumulative"), function(type) {
        read_bvar_raw(sample, order, type) %>%
          dplyr::filter(Impulse == "Gov Exp") %>%
          dplyr::transmute(
            sample = sample,
            sample_label = sample_label,
            order = order,
            order_label = paste("Order", order),
            type = type,
            impulse = Impulse,
            response = Response,
            horizon = Horizon,
            q10 = `10%` * scale * 100,
            q16 = `16%` * scale * 100,
            q50 = `50%` * scale * 100,
            q84 = `84%` * scale * 100,
            q90 = `90%` * scale * 100
          )
      })
    })
  }
)

readr::write_csv(bvar_data, file.path(out_dir, "bvar_irf.csv"))

lp_samples <- tibble::tribble(
  ~sample,     ~sample_label,
  "official",  "Official 2014-2023",
  "long",      "Long 2002-2023"
)

lp_labels <- c(
  ln_gov_exp = "Government expenditure",
  ln_imports = "Imports",
  ln_gdp_non = "Non-oil GDP",
  ln_cpi = "CPI",
  ln_credit = "Private credit"
)

lp_data <- purrr::pmap_dfr(
  lp_samples,
  function(sample, sample_label) {
    raw <- readr::read_csv(
      file.path(data_dir, sprintf("lp_irf_%s.csv", sample)),
      show_col_types = FALSE
    )

    missing_labels <- setdiff(unique(raw$response), names(lp_labels))
    if (length(missing_labels) > 0) {
      stop("Unmapped LP responses: ", paste(missing_labels, collapse = ", "))
    }

    raw %>%
      dplyr::transmute(
        sample = sample,
        sample_label = sample_label,
        response = response,
        response_label = unname(lp_labels[response]),
        horizon = horizon,
        irf_pct = irf_1pct * 100,
        lower68_pct = lower68 * 100,
        upper68_pct = upper68 * 100,
        lower90_pct = lower90 * 100,
        upper90_pct = upper90 * 100,
        n_obs = n_obs
      )
  }
)

readr::write_csv(lp_data, file.path(out_dir, "lp_irf.csv"))

message("Dashboard data written to ", out_dir, ".")
