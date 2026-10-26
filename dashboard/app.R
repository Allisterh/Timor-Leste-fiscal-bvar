rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, shiny, plotly)

if (!file.exists("bvar_irf.csv") || !file.exists("lp_irf.csv")) {
  stop("Dashboard data not found. Run scripts/build_dashboard_data.R from the repository root.")
}

bvar_data <- readr::read_csv("bvar_irf.csv", show_col_types = FALSE)
lp_data <- readr::read_csv("lp_irf.csv", show_col_types = FALSE)

bvar_sample_levels <- c(
  "Official 2014-2023 (3-variable)",
  "Long 2002-2023 (3-variable)"
)

lp_sample_levels <- c(
  "Official 2014-2023",
  "Long 2002-2023"
)

bvar_data <- bvar_data %>%
  dplyr::mutate(
    sample_label = factor(sample_label, levels = bvar_sample_levels),
    order_label = factor(order_label, levels = c("Order 1", "Order 2"))
  )

lp_data <- lp_data %>%
  dplyr::mutate(
    sample_label = factor(sample_label, levels = lp_sample_levels),
    response_label = factor(
      response_label,
      levels = c("Imports", "Non-oil GDP", "CPI", "Private credit", "Government expenditure")
    )
  )

bvar_labels <- c(
  "Gov Exp" = "Government expenditure",
  "Imports" = "Imports",
  "N.O. GDP" = "Non-oil GDP"
)
bvar_responses <- c("Gov Exp", "Imports", "N.O. GDP")
lp_responses <- c("Imports", "Non-oil GDP", "CPI", "Private credit")

ui <- fluidPage(
  title = "Timor-Leste Fiscal Transmission Dashboard",
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; }
      .well { background-color: #ffffff; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .nav-tabs > li > a { font-weight: 600; }
    "))
  ),

  titlePanel(h2("Timor-Leste Fiscal Transmission Dashboard", align = "center")),
  h4("BVAR and local projections, official versus interpolated data", align = "center"),
  br(),

  tabsetPanel(
    tabPanel(
      "BVAR",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            "bvar_sample", "Sample:",
            choices = bvar_sample_levels,
            selected = bvar_sample_levels[1]
          ),
          selectInput(
            "bvar_type", "IRF type:",
            choices = c("Standard IRF" = "standard", "Cumulative IRF" = "cumulative")
          ),
          selectInput(
            "bvar_response", "Response variable:",
            choices = stats::setNames(bvar_responses, bvar_labels[bvar_responses]),
            selected = "Imports"
          ),
          hr(),
          helpText(
            "Shock: a 1% increase in government expenditure.",
            "Responses are reported in percent.",
            "BVAR horizon 1 is the impact quarter.",
            "Order 1 places government expenditure first; order 2 places GDP first.",
            "Bands: 68% and 90% credible intervals."
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("bvar_plot", height = "650px")
        )
      )
    ),

    tabPanel(
      "Local projections",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            "lp_sample", "Sample:",
            choices = lp_sample_levels,
            selected = lp_sample_levels[1]
          ),
          selectInput(
            "lp_response", "Response variable:",
            choices = lp_responses,
            selected = "Imports"
          ),
          hr(),
          helpText(
            "Local projections use the recursively identified government expenditure shock from a three-variable VAR.",
            "The shock is normalized so that the impact response of government expenditure is 1%.",
            "Horizon 0 is the impact quarter.",
            "Bands: 68% and 90% Newey-West HAC confidence intervals."
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("lp_plot", height = "650px")
        )
      )
    ),

    tabPanel(
      "About",
      fluidRow(
        column(
          width = 8, offset = 2,
          h4("What this dashboard shows"),
          p(
            "The dashboard compares a three-variable Bayesian VAR and local projections for Timor-Leste.",
            "The official sample uses quarterly national accounts from INETL for 2014Q1-2023Q4.",
            "The long sample uses annual ADB data disaggregated to quarterly frequency with the Denton-Cholette method for 2002Q1-2023Q4."
          ),
          h4("Units and interpretation"),
          tags$ul(
            tags$li("All responses are scaled to a 1% increase in government expenditure and reported in percent."),
            tags$li("BVAR horizons run from 1 to 12; horizon 1 is the impact quarter."),
            tags$li("Local-projection horizons run from 0 to 12; horizon 0 is the impact quarter."),
            tags$li("BVAR bands are 68% and 90% posterior credible intervals."),
            tags$li("Local-projection bands are 68% and 90% Newey-West HAC confidence intervals.")
          ),
          h4("Caveats"),
          p(
            "The official sample is short, and the long sample is interpolated.",
            "The dashboard is a descriptive and model-based comparison, not a policy counterfactual.",
            "The five-variable official BVAR is unstable and is therefore not shown here."
          ),
          h4("Source and code"),
          p(
            "Data: INETL, ADB, BCTL, and DNE.",
            "Code and documentation:",
            tags$a(
              href = "https://github.com/Emerson-nic/Timor-Leste-fiscal-bvar",
              "Timor-Leste fiscal BVAR repository",
              target = "_blank"
            ),
            "."
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  output$bvar_plot <- renderPlotly({
    d <- bvar_data %>%
      dplyr::filter(
        sample_label == input$bvar_sample,
        type == input$bvar_type,
        impulse == "Gov Exp",
        response == input$bvar_response
      ) %>%
      dplyr::mutate(
        order_label = factor(order_label, levels = c("Order 1", "Order 2")),
        text = paste0(
          "Order: ", order_label,
          "\nHorizon: ", horizon,
          "\nMedian: ", round(q50, 2), "%",
          "\n68% CI: [", round(q16, 2), ", ", round(q84, 2), "]",
          "\n90% CI: [", round(q10, 2), ", ", round(q90, 2), "]"
        )
      )

    validate(need(nrow(d) > 0, "No observations for this selection."))

    type_label <- if (input$bvar_type == "cumulative") "Cumulative IRF" else "IRF"
    response_name <- unname(bvar_labels[input$bvar_response])

    p <- ggplot2::ggplot(d, ggplot2::aes(x = horizon, y = q50)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q10, ymax = q90, fill = "90% CI"),
        alpha = 0.16
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q16, ymax = q84, fill = "68% CI"),
        alpha = 0.26
      ) +
      ggplot2::geom_point(
        ggplot2::aes(color = "Median", text = text),
        size = 2.0,
        show.legend = FALSE
      ) +
      ggplot2::geom_line(
        ggplot2::aes(color = "Median"),
        linewidth = 2.0,
        lineend = "round",
        show.legend = TRUE
      ) +
      ggplot2::facet_wrap(~ order_label, nrow = 1, scales = "free_y") +
      ggplot2::scale_fill_manual(
        name = "",
        values = c("90% CI" = "grey78", "68% CI" = "grey55")
      ) +
      ggplot2::scale_color_manual(
        name = "",
        values = c("Median" = "#003366")
      ) +
      ggplot2::scale_x_continuous(breaks = 1:12) +
      ggplot2::labs(
        title = paste(type_label, "of government expenditure on", response_name),
        subtitle = paste0("Sample: ", input$bvar_sample, " | Units: percent | BVAR h1 = impact"),
        x = "Horizon (quarters)",
        y = "Response (%)"
      ) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(face = "bold", size = 14),
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 18),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
        panel.border = ggplot2::element_rect(color = "grey80", fill = NA),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal"
      )

    suppressWarnings(plotly::ggplotly(p, tooltip = "text")) %>%
      plotly::layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
  })

  output$lp_plot <- renderPlotly({
    d <- lp_data %>%
      dplyr::filter(
        sample_label == input$lp_sample,
        response_label == input$lp_response
      ) %>%
      dplyr::mutate(
        text = paste0(
          "Horizon: ", horizon,
          "\nEstimate: ", round(irf_pct, 2), "%",
          "\n68% CI: [", round(lower68_pct, 2), ", ", round(upper68_pct, 2), "]",
          "\n90% CI: [", round(lower90_pct, 2), ", ", round(upper90_pct, 2), "]",
          "\nObservations: ", n_obs
        )
      )

    validate(need(nrow(d) > 0, "No observations for this selection."))

    p <- ggplot2::ggplot(d, ggplot2::aes(x = horizon, y = irf_pct)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lower90_pct, ymax = upper90_pct, fill = "90% CI"),
        alpha = 0.16
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lower68_pct, ymax = upper68_pct, fill = "68% CI"),
        alpha = 0.26
      ) +
      ggplot2::geom_point(
        ggplot2::aes(color = "Estimate", text = text),
        size = 2.2,
        show.legend = FALSE
      ) +
      ggplot2::geom_line(
        ggplot2::aes(color = "Estimate"),
        linewidth = 2.1,
        lineend = "round",
        show.legend = TRUE
      ) +
      ggplot2::scale_fill_manual(
        name = "",
        values = c("90% CI" = "grey78", "68% CI" = "grey55")
      ) +
      ggplot2::scale_color_manual(
        name = "",
        values = c("Estimate" = "#003366")
      ) +
      ggplot2::scale_x_continuous(breaks = seq(0, 12, 2)) +
      ggplot2::labs(
        title = paste("Local projection of", input$lp_response),
        subtitle = paste0("Sample: ", input$lp_sample, " | Units: percent | LP h0 = impact"),
        x = "Horizon (quarters)",
        y = "Response (%)"
      ) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 18),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
        panel.border = ggplot2::element_rect(color = "grey80", fill = NA),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal"
      )

    suppressWarnings(plotly::ggplotly(p, tooltip = "text")) %>%
      plotly::layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
  })
}

shinyApp(ui = ui, server = server)
