rm(list = ls())
#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               shiny,
               plotly
               )

#ui ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; }
      .well { background-color: #ffffff; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    "))
  ),
  
  titlePanel(h2("BVAR Impulse-Response Function Dashboard", align = "center")),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("irf_type", "Select IRF Type:",
                  choices = c("Standard IRF" = "standard",
                              "Cumulative IRF" = "cumulative")),
      selectInput("impulse", "Impulse (Shock):", choices = NULL),
      selectInput("response", "Response Variable:", choices = NULL),
      hr(),
      h5("Legend Notes:"),
      tags$ul(
        tags$li("Red ribbon: 80% Confidence Interval"),
        tags$li("Red dots: Significant at 80%"),
        tags$li("Black ribbon: 68% Confidence Interval"),
        tags$li("Black dots: Significant at 68%"),
        tags$li("Black dots: Significant at 68%"),
        tags$li("Variables are in natural logarithms. Monetary variables 
                (except CPI) are measured in millions of US dollars; CPI is 
                an index.")
      ), 
      hr(),
      tags$a(href = "https://github.com/Emerson-nic/Timor-Leste-fiscal-bvar",
             icon("github"), " For more info visit at GitHub",
             target = "_blank",
             style = "color: #2c3e50; font-weight: bold; text-decoration: none;")
    ),
    mainPanel(
      width = 9,
      plotlyOutput("irf_plot", height = "650px")
    )
  )
)



#server logic ----
server <- function(input, output, session) {
  
  #dynamically load data based on user selection
  data_list <- reactive({
    req(input$irf_type)
    
    if (input$irf_type == "standard") {
      df1 <- readr::read_csv("irf_full_order_1.csv", show_col_types = FALSE)
      df2 <- readr::read_csv("irf_full_order_2.csv", show_col_types = FALSE)
    } else {
      df1 <- readr::read_csv("irf_cumulative_order_1.csv", show_col_types = FALSE)
      df2 <- readr::read_csv("irf_cumulative_order_2.csv", show_col_types = FALSE)
    }
    
    list(df1 = df1, df2 = df2)
  })
  
  #update dropdown menus automatically based on csv
  observe({
    req(data_list())
    df <- data_list()$df1
    updateSelectInput(session, "impulse", choices = unique(df$Impulse), selected = "Gov Exp")
    updateSelectInput(session, "response", choices = unique(df$Response), selected = "N.O. GDP")
  })
  
  #plotly output
  output$irf_plot <- renderPlotly({
    req(input$impulse, input$response, data_list())
    
    #filter
    df1 <- data_list()$df1 %>% 
      dplyr::filter(Impulse == input$impulse, Response == input$response)
    df2 <- data_list()$df2 %>% 
      dplyr::filter(Impulse == input$impulse, Response == input$response)
    
    label_1 <- paste0("Order 1: ", input$impulse, " \u2192 ", input$response)
    label_2 <- paste0("Order 2: ", input$impulse, " \u2192 ", input$response)
    
    df1$Order <- label_1
    df2$Order <- label_2
    
    df_plot <- dplyr::bind_rows(df1, df2)
    df_plot$Order <- factor(df_plot$Order, levels = c(label_1, label_2))
    
    #significance flags
    df_plot <- df_plot %>%
      dplyr::mutate(
        sig_80 = ifelse(sign(`10%`) == sign(`90%`) & `10%` != 0, `50%`, NA),
        sig_68 = ifelse(sign(`16%`) == sign(`84%`) & `16%` != 0 & is.na(sig_80), `50%`, NA)
      )
    
    #add text for tooltip
    df_plot <- df_plot %>%
      dplyr::mutate(
        text = paste0(
          "Horizon: ", Horizon, "\n",
          "Median: ", round(`50%`, 4), "\n",
          "80% CI: [", round(`10%`, 4), ", ", round(`90%`, 4), "]\n",
          "68% CI: [", round(`16%`, 4), ", ", round(`84%`, 4), "]"
        )
      )
    
    #build ggplot
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Horizon)) +
      #baseline 0
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
      
      #80% CI Ribbon (Red)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `10%`, ymax = `90%`, fill = "80% CI"), alpha = 0.15) +
      #68% CI Ribbon (Black)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `16%`, ymax = `84%`, fill = "68% CI"), alpha = 0.25) +
      
      #median line 
      ggplot2::geom_line(ggplot2::aes(y = `50%`, color = "Median"), linewidth = 1.2, na.rm = TRUE) +
      
      #significance points 
      ggplot2::geom_point(ggplot2::aes(y = sig_68, text = text), 
                          color = "black", size = 3.5, na.rm = TRUE, show.legend = FALSE) +
      ggplot2::geom_point(ggplot2::aes(y = sig_80, text = text),
                          color = "red", size = 3.5, na.rm = TRUE, show.legend = FALSE) +
      
      #faceting
      ggplot2::facet_wrap(~ Order) +
      
      #scales
      ggplot2::scale_fill_manual(name = "", values = c("80% CI" = "red", "68% CI" = "black")) +
      ggplot2::scale_color_manual(name = "", values = c("Median" = "#2c3e50")) +
      
      ggplot2::labs(
        title = paste(if(input$irf_type == "cumulative") "Cumulative Response:" else "Response:", 
                      "Shock of", input$impulse, "on", input$response),
        y = paste(input$response, "Response"),
        x = "Horizon (quarters)"
      ) +
      
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(face = "bold", size = 14),
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 18),
        panel.border = ggplot2::element_rect(color = "grey80", fill = NA),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal"
      )
    
    #convert to plotly
    plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
  })
}

#run the app ----
shinyApp(ui = ui, server = server)
