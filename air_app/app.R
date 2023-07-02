## R Shiny Dashboard of “Calidad de aire” ##

# Load libraries
library(tidyverse)
library(zoo)
library(ggplot2)
library(plotly)
library(htmltools)

# Shiny
library(shiny)
library(shinydashboard)
library(shinycssloaders)

# Tidymodeling
library(tidymodels)
library(modeltime)
library(modeltime.resample)

# Base Models
library(earth)
library(glmnet)
library(xgboost)
library(lightgbm)

# Core Packages
library(tidyverse)
library(lubridate)
library(timetk)
library(bonsai)



no2_zona <- read_rds(here::here("data/no2_zona.RDS"))
models <- read_rds(here::here("data/models.RDS"))
nam_zonas <- no2_zona$zona %>% unique() %>% sort()



# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  title = "Calidad de aire",
  dashboardHeader(title = "Calidad de aire"),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    
    ## Section 1 ----
    # Select input data
    selectInput("zona_i", "Zona", choices = nam_zonas)
    
    
    
    
  ),
  # Corpus
  dashboardBody(
  
  fluidRow(
    box(plotly::plotlyOutput("plot_1"), 
        title = "Evolución del contaminente", 
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12
    )
  ),
  fluidRow(
    box(plotly::plotlyOutput("plot_2"), 
        title = "Media anual del contaminante", 
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 6
    ),
    box(tableOutput("summary_table"), 
        title = "Estadísticas en eu último año", 
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 6
    )
  ),
  fluidRow(
    box(
      title = "Gráfico de precisión",
      width = 6,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      plotOutput('resample_plot') %>%
        withSpinner()
    ),
    box(
      title = "Tabla de precisión",
      width = 6,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      tableOutput('resample_table') %>%
        withSpinner()
    ),
    box(
      title = "Tabla de modelos",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      plotlyOutput('forecast_plot') %>%
        withSpinner()
    )
  )
  )
)


# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  ## Data ----
  # Upload
  dataInput <- reactive({
    no2_zona %>% 
      ungroup() %>% 
      filter(zona == input$zona_i)
      
  })
  
  
  ## Plots ---
  # Plot 1
  output$plot_1 <- renderPlotly({
    data <- dataInput()
    nom_zona <- input$zona_i
    plot_1 <- data %>%
      filter(anomaly == FALSE & fecha >= "2018-01-01") %>%
      ggplot(aes(x = fecha, y = valor_zona)) +
      geom_line() +
      geom_smooth(size = 0.5, color = "red") +
      labs(
        x = NULL,
        y = "(µg/m3)",
        title =  paste0("Evolución de NO2\n Zona: ", nom_zona)
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(plot_1) %>% 
      layout(margin = list(l = 50, r = 50, b = 100, t = 50),
             annotations = list(x = 1, y = -0.3, text = "Fuente: Portal de datos abiertos del Ayuntamiento de Madrid.",
                                xref='paper', yref='paper', showarrow = F, 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font = list(size = 10)))
  })
  
  # Plot 2
  output$plot_2 <- renderPlotly({
    data <- dataInput()
    nom_zona <- input$zona_i
    plot_2 <- data %>% 
      filter(fecha >= "2014-01-01" & anomaly == FALSE) %>% 
      ggplot(aes(fecha, valor_zona_anual)) +
      geom_line() +
      scale_x_date() +
      geom_hline(yintercept = 40, color = "red") +
      labs(title = "Cumplimiento del tope de 40 µg/m3 de NO2\nmedia móvil",
           x = NULL,
           y = "µg/m3 de NO2") +
      theme(legend.position = "bottom")
    ggplotly(plot_2) %>% 
      layout(margin = list(l = 50, r = 50, b = 100, t = 50),
             annotations = list(x = 1, y = -0.3, text = "Fuente: Portal de datos abiertos del Ayuntamiento de Madrid.",
                                xref='paper', yref='paper', showarrow = F, 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font = list(size = 10)))
  })
  
  output$resample_plot <- renderPlot({
    nom_zona <- input$zona_i
    models[[nom_zona]]$resample_plot
  })
  
  output$forecast_plot <-  renderPlotly({
    nom_zona <- input$zona_i
    models[[nom_zona]]$forecast_plot
  })
  
  # Tables
  
  output$summary_table <- renderTable({
    data <- dataInput()
    data %>% 
      filter(fecha >= (max(fecha) - 365) & anomaly == FALSE) %>% 
      group_by(Fecha = as.yearmon(fecha)) %>% 
      summarise(
        Max = max(valor_zona, na.rm = T), 
        Media = mean(valor_zona, na.rm = T),
        Min = min(valor_zona, na.rm = T))
      
  })
  
  output$resample_table <- renderTable({
    nom_zona <- input$zona_i
    models[[nom_zona]]$resample_table %>%
      table_modeltime_accuracy(.interactive = FALSE)
  })
    
  


}

# Run the application 
shinyApp(ui = ui, server = server)
