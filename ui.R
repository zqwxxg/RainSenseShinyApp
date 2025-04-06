library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(tmap)
library(leaflet)
library(bslib)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#134178 blue
#f0f5fa light blue
#071f4d dark blue
#dad9d6 grey
#f4f3f2 light grey
#96816d brown

my_theme <- bs_theme(
  version = 5,                   
  bootswatch = "flatly",         
  bg = "#f0f5fa",                
  fg = "#134178",                
  primary = "#071f4d",           
  secondary = "#96816d",         
  success = "#B2B2B2",           
  
  base_font = font_google("Raleway"),       
  heading_font = font_google("Montserrat")  
)

anova_sidebar <- function() {
  div(
    selectInput("anova_station", "Select Station:", choices = NULL),
    airYearpickerInput(
      inputId = "anova_year",
      label = "Select Year:",
      minDate = "2018",
      maxDate = "2024",
      view = "years",
      dateFormat = "yyyy",
      autoClose = TRUE
    ),
    actionButton("run_anova", "Run ANOVA", icon = icon("chart-bar"), class = "btn-primary", style = "width:100%")
    
  )
}

poisson_sidebar <- function() {
  div(
    selectInput("poisson_station", "Select Station:", choices = NULL),
    actionButton("run_poisson", "Run Poisson", icon = icon("chart-bar"), class = "btn-primary", style = "width:100%")
  )
}

kriging_sidebar <- function() {
  div(
    
    radioButtons(
      inputId = "kriging-granularity",
      label = "Select Granularity:",
      choices = c("Daily", "Monthly", "Yearly"),
      selected = "Monthly",
      inline = TRUE
    ),
    
    uiOutput("kriging-date_picker"),
    
    actionButton("submit_kriging", "Submit", icon = icon("cloud-rain"), class = "btn-primary", style = "width: 100%")
  )
}

idw_sidebar <- function() {
  div(
    
    radioButtons(
      inputId = "idw-granularity",
      label = "Select Granularity:",
      choices = c("Daily", "Monthly", "Yearly"),
      selected = "Monthly",
      inline = TRUE
    ),
    
    uiOutput("idw-date_picker"),
    
    numericInput("idw_nmax", "Maximum Neighbors (nmax):", value = 5, min = 3, max = 15),
    numericInput("idw_idp", "Inverse Distance Power (idp):", value = 0, min = 0, max = 2, step = 0.1),
    
    actionButton("submit_idw", "Submit", icon = icon("cloud-rain"), class = "btn-primary", style = "width: 100%")
  )
}


clustering_sidebar <- function() {
  div(
    
    selectInput("clust_type", "Clustering Type:",
                choices = c("partitional", "hierarchical", "fuzzy")),
    
    selectInput("clust_distance", "Distance Measure:",
                choices = c("dtw_basic", "softdtw", "sbd", "euclidean", "gak")),
    
    h6("Centroid Method:"),
    verbatimTextOutput("clust_centroid_text"),
    
    sliderInput("num_clusters", "Number of Clusters:",
                min = 2, max = 5, value = 2),
    
    actionButton("run_clustering", "Run Clustering", icon = icon("project-diagram"), class = "btn-primary", style = "width: 100%")
  )
}

forecasting_sidebar <- function() {
  div(
    airDatepickerInput(
      inputId = "forecast_date",
      label = "Select Date:",
      minDate = "2018-01-01",
      maxDate = "2025-12-31",
      dateFormat = "yyyy-MM-dd",
      autoClose = TRUE
    ),
    actionButton("run_forecast", "View Forecast", icon = icon("cloud-sun-rain"), class = "btn-primary", style = "width:100%")
  )
}

decomposition_sidebar <- function() {
  div(
    selectInput("decomp_station", "Select Station:", choices = NULL),
    actionButton("run_decomposition", "View Decomposition", icon = icon("chart-area"), class = "btn-primary", style = "width: 100%")
  )
}

ui <- (
  page_navbar(title = tagList(tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px;")),
              theme = my_theme,
              id = "main_navbar",
              
              header = tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
              ),
              
              # ---------- Home Panel ----------
              nav_panel(
                "Home",
                div(class = "landing-section",
                    h1("Welcome to RainSense"),
                    p("RainSense is an interactive platform designed to help you explore rainfall variability and forecasting patterns across Singapore. Dive into our visual and analytical tools to uncover insights from historical and projected rainfall data."),
                    br(),
                    layout_columns(
                      col_widths = c(4, 4, 4),
                      div(
                        style = "text-align: center",
                        h5(tags$b("Confirmatory Analysis:")),
                        p("Compare daily rainfall across different monsoon seasons and uncover trends in extreme rainfall events through statistical analysis."),
                      ),
                      div(
                        style = "text-align: center",
                        h5(tags$b("Spatial Analysis & Clustering:")),
                        p("Visualize rainfall distribution across regions and identify spatial patterns that may influence flood risks or drought zones."),
                      ),
                      div(
                        style = "text-align: center",
                        h5(tags$b("Forecasting:")),
                        p("Detect rainfall anomalies and generate future rainfall forecasts using data-driven models, helping you anticipate wet or dry periods ahead of time."),
                      )
                    ),
                    br(),
                    div(
                      h5(tags$b("Dataset Preview")),
                      DT::dataTableOutput("weather_table")
                    )
                    # layout_columns(
                    #   col_widths = c(6, 6),
                    #   div(
                    #     h5(tags$b("Dataset Preview")),
                    #     DT::dataTableOutput("weather_table")
                    #   ),
                    #   leafletOutput("station_map")
                    # )
                )
              ),
              
              # ---------- Confirmatory Analysis Tab ----------
              nav_panel(
                title = "Confirmatory Analysis",
                id = "cda_panel",
                navset_card_tab(
                  nav_panel(
                    "ANOVA",
                    layout_sidebar(
                      open = "always",
                      sidebar = sidebar(open = "always",
                                        anova_sidebar()),
                      div(
                        plotlyOutput("anova_plot"),
                        br(),
                        htmlOutput("anova_summary")
                      )
                    )
                  ),
                  
                  nav_panel(
                    "Poisson",
                    layout_sidebar(
                      sidebar = sidebar(open = "always",
                                        poisson_sidebar()),
                      div(
                        plotlyOutput("poisson_plot"),
                        br(),
                        htmlOutput("poisson_summary")
                      )
                    )
                  )
                )
              ),
              
              # ---------- Spatial Analysis Tab ----------
              nav_panel(
                title = "Spatial Analysis",
                id = "spatial_panel",
                navset_card_tab(
                  nav_panel(
                    "Ordinary Kriging",
                    layout_sidebar(
                      sidebar = sidebar(open = "always",
                                        kriging_sidebar()),
                      uiOutput("kriging_results")
                    )
                  ),
                  
                  nav_panel(
                    "IDW Interpolation",
                    layout_sidebar(
                      sidebar = sidebar(open = "always",
                                        idw_sidebar()),
                      uiOutput("idw_results")
                    )
                  )
                )
              ),
              
              # ---------- Forecasting Panel ----------
              nav_panel(
                title = "Clustering",
                id = "clustering_panel",
                layout_sidebar(
                  sidebar = sidebar(open = "always",
                                    clustering_sidebar()),
                  uiOutput("clustering_results")
                )
              ),
              
              # ---------- Forecasting Panel ----------
              nav_panel(
                title = "Forecasting",
                id = "forecasting_panel",
                navset_card_tab(
                  nav_panel(
                    "Decomposition",
                    layout_sidebar(
                      sidebar = sidebar(open = "always",
                                        decomposition_sidebar()),
                      uiOutput("decomp_results")
                    )
                  ),
                  nav_panel(
                    "Forecasting",
                    layout_sidebar(
                      sidebar = sidebar(open = "always",
                                        forecasting_sidebar()),
                      uiOutput("forecasting_results")
                    )
                  )
                )
              )
              
  )
)
