rm(list = ls())
#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               formattable,
               DT
               
)

#load csv ----
irf_order_1_view <- readr::read_csv("Data/csv/irf_full_order_1.csv")
irf_order_2_view <- readr::read_csv("Data/csv/irf_full_order_2.csv")

irf_order_1_view <- irf_order_1_view %>%
  dplyr::select(Impulse, Response, Horizon, dplyr::everything()) %>%
  dplyr::arrange(Impulse, Response, Horizon)

irf_order_2_view <- irf_order_2_view %>%
  dplyr::select(Impulse, Response, Horizon, dplyr::everything()) %>%
  #everything() select all the other columns
  dplyr::arrange(Impulse, Response, Horizon)

#csv rule ---
negative_color <- formattable::formatter("span", 
                                         style = x ~ formattable::style(color = ifelse(x < 0, "red", "black")))

numeric_cols_1 <- names(irf_order_1_view)[sapply(irf_order_1_view, is.numeric)]

format_list_1 <- lapply(stats::setNames(numeric_cols_1, numeric_cols_1), function(x) negative_color)

numeric_cols_2 <- names(irf_order_2_view)[sapply(irf_order_2_view, is.numeric)]

format_list_2 <- lapply(stats::setNames(numeric_cols_2, numeric_cols_2), function(x) negative_color)



## aplicated rule ----

formattable::formattable(irf_order_1_view, format_list_1)

formattable::formattable(irf_order_2_view, format_list_2)

#html ----
DT::datatable(irf_order_1_view, 
              options = list(pageLength = 20),
              filter = 'top', 
              rownames = FALSE) %>%
  DT::formatStyle(
    columns = numeric_cols_1, 
    color = DT::styleInterval(0, c('red', 'black')) 
  )

DT::datatable(irf_order_2_view, 
              options = list(pageLength = 20),
              filter = 'top', 
              rownames = FALSE) %>%
  DT::formatStyle(
    columns = numeric_cols_2, 
    color = DT::styleInterval(0, c('red', 'black')) 
  )

#automation ----

print(irf_order_1_view)
vars_names <- base::unique(irf_order_1_view$Impulse)
base::sink("significant_irf_automatic.md")

#loop
for (imp in vars_names) {
  base::cat(base::paste0("# Impulse ", imp, "\n\n"))
  
  for (resp in vars_names) {
    base::cat(base::paste0("### Response ", resp, "\n\n"))
    
    #filter the data in order 1
    df_sub_1 <- irf_order_1_view %>%
      dplyr::filter(Impulse == imp, Response == resp) %>%
      dplyr::arrange(Horizon)
    
    #filter the data in order 2
    df_sub_2 <- irf_order_2_view %>%
      dplyr::filter(Impulse == imp, Response == resp) %>%
      dplyr::arrange(Horizon)
    
    #significant order 1 (Excluyendo los ceros estructurales con != 0)
    #68% CI
    sig_68_1 <- df_sub_1$Horizon[base::sign(df_sub_1$`16%`) == base::sign(df_sub_1$`84%`) & df_sub_1$`16%` != 0]
    #80% CI
    sig_80_1 <- df_sub_1$Horizon[base::sign(df_sub_1$`10%`) == base::sign(df_sub_1$`90%`) & df_sub_1$`10%` != 0]
    
    #significant order 2
    #68% CI
    sig_68_2 <- df_sub_2$Horizon[base::sign(df_sub_2$`16%`) == base::sign(df_sub_2$`84%`) & df_sub_2$`16%` != 0]
    #80% CI
    sig_80_2 <- df_sub_2$Horizon[base::sign(df_sub_2$`10%`) == base::sign(df_sub_2$`90%`) & df_sub_2$`10%` != 0]
    
    #irf 1 logic
    if (base::length(sig_68_1) == 0) {
      base::cat("IRF 1: No horizons are statistically significant - 0\n\n")
    } else if (base::length(sig_68_1) == base::nrow(df_sub_1) && base::length(sig_80_1) == base::nrow(df_sub_1)) {
      base::cat(base::paste0("IRF 1: All horizons are statistically significant - 1:", base::max(df_sub_1$Horizon), " Both credible intervals\n\n"))
    } else {
      base::cat(base::paste0("IRF 1: From horizon ", base::min(sig_68_1), " to ", base::max(sig_68_1),
                             " are significant 68% credible intervals - ", base::min(sig_68_1), ":", base::max(sig_68_1), "\n\n"))
      if (base::length(sig_80_1) > 0) {
        base::cat(base::paste0("From horizon ", base::min(sig_80_1), " to ", base::max(sig_80_1),
                               " are significant 80% credible intervals - ", base::min(sig_80_1), ":", base::max(sig_80_1), "\n\n"))
      }
    }
    
    #irf 2 logic
    if (base::length(sig_68_2) == 0) {
      base::cat("IRF 2: No horizons are statistically significant - 0\n\n")
    } else if (base::length(sig_68_2) == base::nrow(df_sub_2) && base::length(sig_80_2) == base::nrow(df_sub_2)) {
      base::cat(base::paste0("IRF 2: All horizons are statistically significant - 1:", base::max(df_sub_2$Horizon), " Both credible intervals\n\n"))
    } else {
      base::cat(base::paste0("IRF 2: From horizon ", base::min(sig_68_2), " to ", base::max(sig_68_2),
                             " are significant 68% credible intervals - ", base::min(sig_68_2), ":", base::max(sig_68_2), "\n\n"))
      if (base::length(sig_80_2) > 0) {
        base::cat(base::paste0("From horizon ", base::min(sig_80_2), " to ", base::max(sig_80_2),
                               " are significant 80% credible intervals - ", base::min(sig_80_2), ":", base::max(sig_80_2), "\n\n"))
      }
    }
    
    base::cat(base::paste0("- [ ] Shock ", imp, " on ", resp, "\n\n"))
  }
}

#close file
base::sink()
