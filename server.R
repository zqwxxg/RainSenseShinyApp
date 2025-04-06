library(jsonify)
library(jsonlite)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(dplyr)
library(ggstatsplot)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(terra)
library(tmap)
library(gstat)
library(automap)
library(viridis)
library(dtwclust)
library(feasts)
library(fable)
library(seasonal)

if (!dir.exists("cache")) dir.create("cache")

tmap_mode("view")

# ----------------------------------
# CDA Data

# weather <- read.csv("data/weather.csv")
# weather_active <- read.csv("data/weather_active.csv")
# weather_active$Date <- as.Date(weather_active$Date)
# weather_active$Daily.Rainfall.Total..mm. <- as.numeric(weather_active$Daily.Rainfall.Total..mm.)

# ----------------------------------
# Interpolation data

# weather_sf <- readRDS("data/weather_sf.rds")
# monthly_data_sf <- readRDS("data/monthly_data_sf.rds")
# yearly_data_sf <- readRDS("data/yearly_data_sf.rds")

sg_boundary <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>%
  st_transform(crs = 3414)
# 
grid <- terra::rast(sg_boundary, nrows = 690, ncols = 1075)
# 
# coop <- readRDS("data/coop.rds")

# ----------------------------------
# Clustering data

# monthly_active_data_sf <- readRDS("data/monthly_active_data_sf.rds")
# rain_list <- readRDS("data/rain_list.rds")

# ----------------------------------
# Decomposition data
# 
# tsibble_data <- readRDS("data/tsibble_data.rds")

# ----------------------------------
# Forecasting data

# weather_predicted_sf <- readRDS("data/weather_predicted_sf.rds")

# ----------------------------------
# Main Server


server <- function(input, output, session) {
  
  # ---------- Reactive file readers ----------
  
  weather_data <- reactiveVal()
  weather_active_data <- reactiveVal()
  weather_sf_data <- reactiveVal()
  monthly_data_sf_data <- reactiveVal()
  yearly_data_sf_data <- reactiveVal()
  coop_data <- reactiveVal()
  monthly_active_data_sf_data <- reactiveVal()
  rain_list_data <- reactiveVal()
  tsibble_data_data <- reactiveVal()
  weather_predicted_sf_data <- reactiveVal()
  
  observe({
    tryCatch({
      weather_data(read.csv("data/weather.csv"))
    }, error = function(e) {
      print("?????? Error reading weather.csv:")
      print(e)
    })
    
    tryCatch({
      df <- read.csv("data/weather_active.csv")
      df$Date <- as.Date(df$Date)
      df$Daily.Rainfall.Total..mm. <- as.numeric(df$Daily.Rainfall.Total..mm.)
      weather_active_data(df)
    }, error = function(e) {
      print("?????? Error reading weather_active.csv:")
      print(e)
    })
    
    tryCatch({
      weather_sf_data(readRDS("data/weather_sf.rds"))
      monthly_data_sf_data(readRDS("data/monthly_data_sf.rds"))
      yearly_data_sf_data(readRDS("data/yearly_data_sf.rds"))
      coop_data(readRDS("data/coop.rds"))
      monthly_active_data_sf_data(readRDS("data/monthly_active_data_sf.rds"))
      rain_list_data(readRDS("data/rain_list.rds"))
      tsibble_data_data(readRDS("data/tsibble_data.rds"))
      weather_predicted_sf_data(readRDS("data/weather_predicted_sf.rds"))
    }, error = function(e) {
      print("?????? Error reading RDS files:")
      print(e)
    })
  })
  
  # ----------------------------------
  # Home station map
  
  output$weather_table <- DT::renderDataTable({
    req(weather_data())
    weather <- weather_data()

    DT::datatable(
      weather,
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  station_map_cache <- reactiveVal(NULL)
  station_map_data_file <- "cache/station_map_data.rds"

  output$station_map <- renderLeaflet({
    #  Return cached leaflet if exists in session
    if (!is.null(station_map_cache())) {
      return(station_map_cache())
    }
    
    #  Try load cached data from disk
    if (file.exists(station_map_data_file)) {
      message("??????? Loaded station map data from cache.")
      weather <- readRDS(station_map_data_file)
    } else {
      req(weather_data())
      weather <- weather_data()
      dir.create("cache", showWarnings = FALSE)
      saveRDS(weather, station_map_data_file)
      message("???? Saved station map data to cache.")
    }
    
    # Build the map
    pal <- colorFactor(
      palette = c("Active" = "#00b050", "Inactive" = "red"),
      domain = weather$Status
    )
    
    map <- leaflet(weather) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        color = ~pal(Status),
        radius = 2.5,
        fillOpacity = 0.7,
        popup = ~paste0("Station: ", Station)
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~Status,
        title = "Status",
        opacity = 1
      )
    
    station_map_cache(map)
    map
  })


  # ----------------------------------
  # CDA
  
  # ANOVA

  observe({
    req(weather_active_data())
    weather_active <- weather_active_data()

    station_choices <- c("All", unique(weather_active$Station))
    updateSelectInput(inputId = "anova_station", choices = station_choices)
    updateSelectInput(inputId = "poisson_station", choices = station_choices)
  })
  
  observeEvent(input$run_anova, {
    withProgress(message = "Running ANOVA analysis...", value = 0, {
      
      req(weather_active_data())
      incProgress(0.1)
      
      weather_active <- weather_active_data()
      selected_station <- input$anova_station
      selected_year <- year(input$anova_year)
      
      # Define cache path
      cache_key <- paste0("anova_", gsub(" ", "_", selected_station), "_", selected_year)
      cache_file <- paste0("cache/", cache_key, ".rds")
      
      # If cache exists, load and display
      if (file.exists(cache_file)) {
        message("??????? Loaded ANOVA from cache: ", cache_file)
        cached <- readRDS(cache_file)
        
        output$anova_plot <- renderPlotly({ cached$plot })
        output$anova_summary <- renderUI({ HTML(cached$summary) })
        incProgress(1)
        return()
      }
      
      #  Filter data
      data_filtered <- weather_active %>%
        filter((selected_station == "All" | Station == selected_station), Year == selected_year)
      
      incProgress(0.4)
      
      if (nrow(data_filtered) == 0) {
        output$anova_plot <- renderPlotly({
          ggplotly(ggplot() +
                     annotate("text", x = 1, y = 1, label = "No data found for this station and year") +
                     xlim(0, 2) + ylim(0, 2) + theme_void())
        })
        output$anova_summary <- renderUI({
          HTML(paste0("<div style='color:black;'>No data available for <b>", selected_station,
                      "</b> in <b>", selected_year, "</b>.</div>"))
        })
        return()
      }
      
      incProgress(0.6)
      
      # Generate plot
      anova_plot_obj <- ggbetweenstats(
        data = data_filtered,
        x = Season,
        y = Daily.Rainfall.Total..mm.,
        type = "p",
        pairwise.comparisons = TRUE,
        pairwise.display = "s",
        output = "plot"
      ) + labs(title = paste("ANOVA Result for", selected_station, "in", selected_year))
      
      plotly_obj <- ggplotly(anova_plot_obj)
      
      incProgress(0.8)
      
      # Generate summary
      means <- data_filtered %>%
        group_by(Season) %>%
        summarise(Mean = mean(Daily.Rainfall.Total..mm., na.rm = TRUE)) %>%
        arrange(desc(Mean))
      top <- means[1, ]
      summary_html <- paste0(
        "<div style='color:black;'>",
        "In <b>", selected_year, "</b>, at <b>", selected_station, "</b>, the season with the highest average daily rainfall was <b>",
        top$Season, "</b> with <b>", round(top$Mean, 2), " mm</b>.",
        "</div>"
      )
      
      #  Render outputs
      output$anova_plot <- renderPlotly({ plotly_obj })
      output$anova_summary <- renderUI({ HTML(summary_html) })
      
      # Save to cache
      saveRDS(list(
        plot = plotly_obj,
        summary = summary_html
      ), cache_file)
      
      message("??? Saved ANOVA result to: ", cache_file)
      incProgress(1)
    })
  })
  
  # 
  # # ANOVA
  # observeEvent(input$run_anova, {
  #   withProgress(message = "Running ANOVA analysis...", value = 0, {
  #     
  #     req(weather_active_data())
  #     incProgress(0.1)
  #     
  #     weather_active <- weather_active_data()
  #     selected_station <- input$anova_station
  #     selected_year <- year(input$anova_year)
  #     
  #     data_filtered <- weather_active %>%
  #       filter((selected_station == "All" | Station == selected_station), Year == selected_year)
  #     
  #     incProgress(0.3)
  #     
  #     if (nrow(data_filtered) == 0) {
  #       output$anova_plot <- renderPlotly({
  #         ggplotly(ggplot() +
  #                    annotate("text", x = 1, y = 1, label = "No data found for this station and year") +
  #                    xlim(0, 2) + ylim(0, 2) + theme_void())
  #       })
  #       output$anova_summary <- renderUI({
  #         HTML(paste0("<div style='color:black;'>No data available for <b>", selected_station,
  #                     "</b> in <b>", selected_year, "</b>.</div>"))
  #       })
  #       return()
  #     }
  #     
  #     incProgress(0.6)
  #     
  #     output$anova_plot <- renderPlotly({
  #       p <- ggbetweenstats(
  #         data = data_filtered,
  #         x = Season,
  #         y = Daily.Rainfall.Total..mm.,
  #         type = "p",
  #         pairwise.comparisons = TRUE,
  #         pairwise.display = "s",
  #         output = "plot"
  #       ) + labs(title = paste("ANOVA Result for", selected_station, "in", selected_year))
  #       ggplotly(p)
  #     })
  #     
  #     incProgress(0.9)
  #     
  #     output$anova_summary <- renderUI({
  #       means <- data_filtered %>%
  #         group_by(Season) %>%
  #         summarise(Mean = mean(Daily.Rainfall.Total..mm., na.rm = TRUE)) %>%
  #         arrange(desc(Mean))
  #       top <- means[1, ]
  #       HTML(paste0(
  #         "<div style='color:black;'>",
  #         "In <b>", selected_year, "</b>, at <b>", selected_station, "</b>, the season with the highest average daily rainfall was <b>",
  #         top$Season, "</b> with <b>", round(top$Mean, 2), " mm</b>.",
  #         "</div>"
  #       ))
  #     })
  #     
  #     incProgress(1)
  #   })
  # })

  # Poisson
  observeEvent(input$run_poisson, {
    req(weather_active_data())
    weather_active <- weather_active_data()
    selected_station <- input$poisson_station

    withProgress(message = "Running Poisson Regression...", value = 0, {
      incProgress(0.3, detail = "Filtering extreme rainfall days")
      poisson_data <- weather_active %>%
        filter((selected_station == "All" | Station == selected_station), Daily.Rainfall.Total..mm. > 100) %>%
        count(Year) %>%
        mutate(Year = as.integer(Year))

      if (nrow(poisson_data) == 0 || is.na(sd(poisson_data$Year))) {
        output$poisson_plot <- renderPlotly({
          ggplotly(ggplot() + annotate("text", x = 1, y = 1, label = "Insufficient data") + xlim(0, 2) + ylim(0, 2) + theme_void())
        })
        output$poisson_summary <- renderUI({
          HTML("<div style='color:black;'>No extreme rainfall data available for this station.</div>")
        })
        return()
      }

      incProgress(0.6, detail = "Fitting model")
      model <- glm(n ~ Year, family = "poisson", data = poisson_data)
      coef <- summary(model)$coefficients
      beta <- round(coef["Year", "Estimate"], 4)
      pval <- signif(coef["Year", "Pr(>|z|)"], 3)

      output$poisson_plot <- renderPlotly({
        p <- ggplot(poisson_data, aes(x = Year, y = n)) +
          geom_point(size = 3, color = "#134178") +
          geom_smooth(method = "glm", method.args = list(family = "poisson"), se = FALSE, color = "#fdae61") +
          labs(title = paste("Extreme Rainfall Days Trend for", selected_station),
               y = "Days > 100mm", x = "Year") +
          theme_minimal()
        ggplotly(p)
      })

      output$poisson_summary <- renderUI({
        HTML(paste0(
          "<div style='color:black;'>",
          "Trend coefficient: ", beta, "<br>",
          "p-value: ", pval, "<br>",
          if (pval < 0.05) "<b>Significant trend detected.</b>" else "<b>No significant trend detected.</b>",
          "</div>"
        ))
      })

      incProgress(1)
    })
  })
  
  # ----------------------------------
  # Interpolation
  
  # Kriging
  
  observe({
    gran <- input$`kriging-granularity`

    req(gran)

    output$`kriging-date_picker` <- renderUI({

      if (gran == "Daily") {
        airDatepickerInput(
          inputId = "kriging-selected_date",
          label = "Select Date:",
          minDate = "2018-01-01",
          maxDate = "2024-12-31",
          view = "days",
          dateFormat = "yyyy-MM-dd",
          autoClose = TRUE
        )
      } else if (gran == "Monthly") {
        airMonthpickerInput(
          inputId = "kriging-selected_date",
          label = "Select Month:",
          minDate = "2018-01",
          maxDate = "2024-12",
          view = "months",
          dateFormat = "yyyy-MM",
          autoClose = TRUE
        )
      } else if (gran == "Yearly") {
        airYearpickerInput(
          inputId = "kriging-selected_date",
          label = "Select Year:",
          minDate = "2018",
          maxDate = "2024",
          view = "years",
          dateFormat = "yyyy",
          autoClose = TRUE
        )
      }
    })
  })

  rain_map_result <- reactiveVal()
  rain_selected_date <- reactiveVal()
  rain_selected_gran <- reactiveVal()
  v_auto_result <- reactiveVal()
  
  observeEvent(input$submit_kriging, {
    req(monthly_data_sf_data(), yearly_data_sf_data(), weather_sf_data(), coop_data())
    
    granularity <- input$`kriging-granularity`
    selected_date <- input$`kriging-selected_date`
    
    tif_file <- paste0("cache/kriging_", granularity, "_", selected_date, ".tif")
    meta_file <- paste0("cache/kriging_", granularity, "_", selected_date, ".rds")
    
    withProgress(message = "Running Kriging interpolation...", value = 0, {
      
      # If both raster and metadata exist, load from cache
      if (file.exists(tif_file) && file.exists(meta_file)) {
        message("??????? Loaded from cache: ", tif_file)
        cached <- readRDS(meta_file)
        r <- terra::rast(tif_file)
        
        rain_map_result(r)
        v_auto_result(cached$variogram)
        rain_selected_gran(tolower(granularity))
        rain_selected_date(selected_date)
        return()
      }
      
      incProgress(0.1, detail = "Filtering input...")
      
      df <- switch(granularity,
                   "Daily" = weather_sf_data() %>% filter(Date == selected_date),
                   "Monthly" = monthly_data_sf_data() %>% filter(floor_date(MonthYear, "month") == selected_date),
                   "Yearly" = yearly_data_sf_data() %>% filter(Year == year(selected_date)))
      
      if (nrow(df) == 0) {
        showNotification("??? No data for selected time.", type = "error")
        return()
      }
      
      formula <- switch(granularity,
                        "Daily" = Daily.Rainfall.Total..mm. ~ 1,
                        "Monthly" = MonthlyRain ~ 1,
                        "Yearly" = YearlyRain ~ 1)
      
      incProgress(0.3, detail = "Fitting variogram...")
      
      tryCatch({
        v_auto <- autofitVariogram(formula, df)
        
        incProgress(0.5, detail = "Running kriging...")
        model <- gstat(formula = formula, model = v_auto$var_model, data = df)
        
        coop <- st_transform(coop_data(), crs = st_crs(df))
        pred <- predict(model, coop)
        
        pred$pred <- pred$var1.pred
        
        incProgress(0.7, detail = "Rasterizing...")
        r <- terra::rasterize(pred, grid, field = "pred")
        
        # Save raster as .tif
        terra::writeRaster(r, tif_file, overwrite = TRUE)
        
        # Save metadata separately
        saveRDS(list(variogram = v_auto), file = meta_file)
        
        message("??? Saved raster to: ", tif_file)
        message("??? Saved metadata to: ", meta_file)
        
        rain_map_result(r)
        v_auto_result(v_auto)
        rain_selected_gran(tolower(granularity))
        rain_selected_date(selected_date)
        
        incProgress(1)
      }, error = function(e) {
        showNotification(paste("??? Kriging failed:", e$message), type = "error")
      })
    })
  })
  
  # observeEvent(input$submit_kriging, {
  #   req(monthly_data_sf_data(), yearly_data_sf_data(), weather_sf_data(), coop_data())
  # 
  #   granularity <- input$`kriging-granularity`
  #   selected_date <- input$`kriging-selected_date`
  # 
  #   req(granularity, selected_date)
  # 
  #   # Start progress bar early
  #   withProgress(message = "Running interpolation...", value = 0.1, {
  # 
  #     future({
  #       monthly_data_sf <- isolate(monthly_data_sf_data())
  #       yearly_data_sf <- isolate(yearly_data_sf_data())
  #       weather_sf <- isolate(weather_sf_data())
  #       coop <- isolate(coop_data())
  # 
  #       specific <- switch(granularity,
  #                          "Monthly" = monthly_data_sf_data() %>% filter(floor_date(MonthYear, "month") == selected_date),
  #                          "Daily" = weather_sf_data() %>% filter(format(Date, "%Y-%m-%d") == selected_date),
  #                          "Yearly" = yearly_data_sf_data() %>% filter(Year == year(selected_date))
  #       )
  # 
  #       if (nrow(specific) == 0) return(NULL)
  # 
  #       formula <- switch(granularity,
  #                         "Monthly" = MonthlyRain ~ 1,
  #                         "Daily" = Daily.Rainfall.Total..mm. ~ 1,
  #                         "Yearly" = YearlyRain ~ 1
  #       )
  # 
  #       v_auto <- autofitVariogram(formula, specific)
  #       k <- gstat(formula = formula, model = v_auto$var_model, data = specific)
  #       resp <- predict(k, coop_data())
  # 
  #       resp$x <- st_coordinates(resp)[, 1]
  #       resp$y <- st_coordinates(resp)[, 2]
  #       resp$pred <- resp$var1.pred
  # 
  #       kpred <- terra::rasterize(resp, grid, field = "pred")
  # 
  #       list(raster = kpred, v_auto = v_auto, gran = tolower(granularity), date = selected_date)
  #     }) %...>% {
  #       result <- .
  # 
  #       if (is.null(result)) {
  #         showNotification("No data available for selected time.", type = "error")
  #         return()
  #       }
  # 
  #       rain_map_result(result$raster)
  #       v_auto_result(result$v_auto)
  #       rain_selected_gran(result$gran)
  #       rain_selected_date(result$date)
  # 
  #       incProgress(1, detail = "Done")
  #     } %...!% {
  #       showNotification(paste("Kriging failed:", .), type = "error")
  #     }
  #   })
  # })
  
  output$rain_map <- renderPlot({
    req(rain_map_result())
    req(rain_selected_date())
    req(rain_selected_gran())
    
    raster_data <- rain_map_result()
    
    if (!inherits(raster_data, "SpatRaster")) {
      showNotification("??? Not a valid raster", type = "error")
      return()
    }
    
    if (is.null(terra::crs(raster_data))) {
      showNotification("??? Missing CRS", type = "error")
      return()
    }
    
    if (any(is.na(terra::minmax(raster_data)))) {
      showNotification("??? Raster contains only NA values", type = "error")
      return()
    }
    
    title_date <- switch(
      rain_selected_gran(),
      "monthly" = format(rain_selected_date(), "%Y %b"),
      "yearly"  = format(rain_selected_date(), "%Y"),
      "daily"   = format(rain_selected_date(), "%d %b %Y")
    )
    
    tmap_mode("plot")
    
    tryCatch({
      map <- tm_shape(raster_data) +
        tm_raster(
          col.scale = tm_scale_continuous(values = "brewer.blues"),
          col.legend = tm_legend(title = "Rainfall (mm)")
        ) +
        tm_title(text = paste("Rainfall Distribution in", title_date)) +
        tm_layout(frame = TRUE) +
        tm_compass(type = "8star", size = 2) +
        tm_scalebar(position = c("left", "bottom")) +
        tm_grid(alpha = 0.2)
      
      print(map)
    }, error = function(e) {
      showNotification(paste("Plotting failed:", e$message), type = "error")
    })
  })
  
# output$rain_map <- renderPlot({
#   req(rain_map_result())
#   req(rain_selected_date())
#   req(rain_selected_gran())
# 
#   # Check class of raster object
#   raster_data <- rain_map_result()
# 
#   if (!inherits(raster_data, "SpatRaster")) {
#     showNotification("Rainfall raster is not a valid `terra` SpatRaster.", type = "error")
#     return()
#   }
# 
#   # Build title
#   title_date <- switch(
#     rain_selected_gran(),
#     "monthly" = format(rain_selected_date(), "%Y %b"),
#     "yearly"  = format(rain_selected_date(), "%Y"),
#     "daily"   = format(rain_selected_date(), "%d %b %Y")
#   )
# 
#   # Plot safely
#   tmap_mode("plot")
# 
#   tryCatch({
#     map <- tm_shape(raster_data) +
#       tm_raster(
#         col.scale = tm_scale_continuous(values = "brewer.blues"),
#         col.legend = tm_legend(title = "Total rainfall (mm)")
#       ) +
#       tm_title(text = paste("Rainfall Distribution in", title_date)) +
#       tm_layout(frame = TRUE) +
#       tm_compass(type = "8star", size = 2) +
#       tm_scalebar(position = c("left", "bottom")) +
#       tm_grid(alpha = 0.2)
# 
#     print(map)
#   }, error = function(e) {
#     showNotification(paste("Plotting failed:", e$message), type = "error")
#   })
# })
  
  output$kriging_summary <- renderUI({
    HTML(paste0(
      "<div style='color:black;'>",
      "This map uses <b>Ordinary Kriging</b> to visualize the spatial distribution of total rainfall. ",
      "Ordinary Kriging is a geostatistical method that estimates rainfall in unmeasured areas based on known station data and their spatial relationships. ",
      "<b>Darker areas</b> indicate regions with higher estimated rainfall, while <b>lighter areas</b> suggest lower rainfall.",
      "</div>"
    ))
  })
  
  output$v_auto_plot <- renderPlot({
    req(v_auto_result())
    plot(v_auto_result())
  })
  
  output$variogram_summary <- renderUI({
    HTML(paste0(
      "<div style='color:black;'>",
      "The variogram helps determine how spatially correlated rainfall values are over distance.",
      "A good fit improves the accuracy of spatial predictions.<br><br>",
      "Model Parameters:<br>",
      "<b>Nugget</b>: Measurement error or small-scale variability.<br>",
      "<b>Sill</b>: The level at which the variogram levels off - the total variance.<br>",
      "<b>Range</b>: The distance beyond which points are no longer spatially correlated.",
      "</div>"
    ))
  })
  
  
  output$kriging_results <- renderUI({
    req(rain_map_result())
    req(v_auto_result)
    
    div(
      h4(tags$b("Ordinary Kringing Map")),
      plotOutput("rain_map"),
      br(),
      htmlOutput("kriging_summary"),
      
      br(),
      br(),
      
      h4(tags$b("Autofitted Variogram")),
      plotOutput("v_auto_plot"),
      br(),
      htmlOutput("variogram_summary")
    )
  })
  
  # IDW
  observe({
    gran <- input$`idw-granularity`

    req(gran)

    output$`idw-date_picker` <- renderUI({

      if (gran == "Daily") {
        airDatepickerInput(
          inputId = "idw-selected_date",
          label = "Select Date:",
          minDate = "2018-01-01",
          maxDate = "2024-12-31",
          view = "days",
          dateFormat = "yyyy-MM-dd",
          autoClose = TRUE
        )
      } else if (gran == "Monthly") {
        airMonthpickerInput(
          inputId = "idw-selected_date",
          label = "Select Month:",
          minDate = "2018-01",
          maxDate = "2024-12",
          view = "months",
          dateFormat = "yyyy-MM",
          autoClose = TRUE
        )
      } else if (gran == "Yearly") {
        airYearpickerInput(
          inputId = "idw-selected_date",
          label = "Select Year:",
          minDate = "2018",
          maxDate = "2024",
          view = "years",
          dateFormat = "yyyy",
          autoClose = TRUE
        )
      }
    })
  })

  idw_map_result <- reactiveVal()
  idw_selected_gran <- reactiveVal()
  idw_selected_date <- reactiveVal()

  observeEvent(input$submit_idw, {
    req(monthly_data_sf_data(), yearly_data_sf_data(), weather_sf_data(), coop_data())
    
    granularity <- input$`idw-granularity`
    selected_date <- input$`idw-selected_date`
    nmax <- input$idw_nmax
    idp <- input$idw_idp
    
    # Define safe filenames for raster and metadata
    idw_key <- paste0("idw_", granularity, "_", selected_date, "_nmax", nmax, "_idp", idp)
    tif_file <- paste0("cache/", idw_key, ".tif")
    meta_file <- paste0("cache/", idw_key, ".rds")
    
    withProgress(message = "Running IDW interpolation...", value = 0, {
      
      # ??? Check if cached .tif + .rds exists
      if (file.exists(tif_file) && file.exists(meta_file)) {
        message("??????? Loaded IDW cache: ", tif_file)
        cached <- readRDS(meta_file)
        r <- terra::rast(tif_file)
        
        idw_map_result(r)
        idw_selected_gran(tolower(granularity))
        idw_selected_date(selected_date)
        return()
      }
      
      incProgress(0.1, detail = "Filtering data...")
      
      df <- switch(granularity,
                   "Daily" = weather_sf_data() %>% filter(Date == selected_date),
                   "Monthly" = monthly_data_sf_data() %>% filter(floor_date(MonthYear, "month") == selected_date),
                   "Yearly" = yearly_data_sf_data() %>% filter(Year == year(selected_date)))
      
      if (nrow(df) == 0) {
        showNotification("??? No data available for this date", type = "error")
        return()
      }
      
      formula <- switch(granularity,
                        "Daily" = Daily.Rainfall.Total..mm. ~ 1,
                        "Monthly" = MonthlyRain ~ 1,
                        "Yearly" = YearlyRain ~ 1)
      
      incProgress(0.3, detail = "Fitting IDW model...")
      
      tryCatch({
        coop <- st_transform(coop_data(), crs = st_crs(df))
        model <- gstat(formula = formula, locations = df, nmax = nmax, set = list(idp = idp))
        pred <- predict(model, coop)
        
        pred$pred <- pred$var1.pred
        
        incProgress(0.7, detail = "Rasterizing...")
        r <- terra::rasterize(pred, grid, field = "pred", fun = "mean")
        
        # ??? Save raster and metadata
        terra::writeRaster(r, tif_file, overwrite = TRUE)
        saveRDS(list(), meta_file)
        
        idw_map_result(r)
        idw_selected_gran(tolower(granularity))
        idw_selected_date(selected_date)
        
        message("??? IDW raster saved: ", tif_file)
        incProgress(1)
      }, error = function(e) {
        showNotification(paste("??? IDW failed:", e$message), type = "error")
      })
    })
  })
  
  output$idw_map <- renderPlot({
    req(idw_map_result())
    req(idw_selected_date())
    req(idw_selected_gran())
    
    r <- idw_map_result()
    
    if (!inherits(r, "SpatRaster")) {
      showNotification("??? IDW raster is not valid", type = "error")
      return()
    }
    
    if (is.null(terra::crs(r))) {
      showNotification("??? IDW raster has no CRS", type = "error")
      return()
    }
    
    if (any(is.na(terra::minmax(r)))) {
      showNotification("??? IDW raster contains no values", type = "error")
      return()
    }
    
    title_date <- switch(
      idw_selected_gran(),
      "monthly" = format(idw_selected_date(), "%Y %b"),
      "yearly"  = format(idw_selected_date(), "%Y"),
      "daily"   = format(idw_selected_date(), "%d %b %Y")
    )
    
    tmap_mode("plot")
    
    tryCatch({
      map <- tm_shape(r) +
        tm_raster(
          col.legend = tm_legend(title = "Total rainfall (mm)")
        ) +
        tm_title(text = paste("Rainfall Distribution in", title_date)) +
        tm_layout(frame = TRUE) +
        tm_compass(type = "8star", size = 2) +
        tm_scalebar(position = c("left", "bottom")) +
        tm_grid(alpha = 0.2)
      print(map)
    }, error = function(e) {
      showNotification(paste("??? IDW plot failed:", e$message), type = "error")
    })
  })
  
  # output$idw_map <- renderPlot({
  #   req(idw_map_result())
  #   req(idw_selected_date())
  #   req(idw_selected_gran())
  # 
  #   title_date <- if (idw_selected_gran() == "monthly") {
  #     format(idw_selected_date(), "%Y %b")
  #   } else if (idw_selected_gran() == "yearly") {
  #     format(idw_selected_date(), "%Y")
  #   }
  # 
  #   tmap_mode("plot")
  # 
  #   map <- tm_shape(idw_map_result()) +
  #     tm_raster(
  #       col.legend = tm_legend(title = "Total rainfall (mm)")
  #     ) +
  #     tm_title(text = paste("Rainfall Distribution in", title_date)) +
  #     tm_layout(frame = TRUE) +
  #     tm_compass(type = "8star", size = 2) +
  #     tm_scalebar(position = c("left", "bottom")) +
  #     tm_grid(alpha = 0.2)
  # 
  #   print(map)
  # })

  output$idw_summary <- renderUI({
    HTML(paste0(
      "<div style='color:black;'>",
      "This map uses <b>Inverse Distance Weighting (IDW)</b> to estimate rainfall based on surrounding weather stations. ",
      "IDW assumes that rainfall at unknown locations is more influenced by nearby stations than distant ones.<br><br>",
      "<b>nmax</b>: The maximum number of neighboring stations considered for each prediction.<br>",
      "Higher values use more stations (smoother results), while lower values make estimates more localized.<br><br>",
      "<b>idp</b>: The inverse distance power determines how much weight is given to closer stations.<br>",
      "Higher values emphasize nearby stations (sharper transitions), while lower values create smoother gradients.",
      "</div>"
    ))
  })

  output$idw_results <- renderUI({
    req(idw_map_result())

    div(
      h4(tags$b("IDW Interpolation Map")),
      plotOutput("idw_map"),
      br(),
      htmlOutput("idw_summary")
    )
  })

  # ----------------------------------
  # Clustering

  cluster_map_result <- reactiveVal()
  cluster_model <- reactiveVal()

  centroid_method <- reactive({
    switch(input$clust_distance,
           "dtw_basic" = "dba",
           "sbd" = "shape",
           "euclidean" = "mean",
           "gak" = "pam")
  })

  output$clust_centroid_text <- renderText({
    req(centroid_method())
    centroid_method()
  })

  observeEvent(input$run_clustering, {
    req(monthly_active_data_sf_data(), rain_list_data())

    monthly_active_data_sf <- monthly_active_data_sf_data()
    rain_list <- rain_list_data()

    withProgress(message = "Running clustering...", value = 0, {
      type <- input$clust_type
      k <- input$num_clusters
      distance <- input$clust_distance

      req(type, k, distance)

      incProgress(0.3, detail = "Fitting model...")

      model <- tsclust(
        series = rain_list,
        type = type,
        k = k,
        distance = distance,
        centroid = centroid_method(),
        seed = 123
      )

      cluster_model(model)

      station_clusters <- data.frame(
        Station = names(rain_list),
        Cluster = as.factor(model@cluster)
      )

      clustered_stations <- monthly_active_data_sf %>%
        left_join(station_clusters, by = "Station")

      incProgress(0.7, detail = "Generating map...")

      tmap_mode("view")

      map <- tm_shape(sg_boundary) +
        tm_polygons(col = "grey90", border.col = "white") +
        tm_shape(clustered_stations) +
        tm_symbols(
          col = "Cluster",
          palette = "hcl.set2",
          size = 0.7,
          size.scale = tm_scale(1),
          col.legend = tm_legend("Cluster"),
          popup.vars = "Station",
          interactive = TRUE
        ) +
        tm_layout(
          legend.outside = TRUE,
          frame = FALSE
        ) +
        tm_title("Time Series Clusters by Station") +
        tm_view(basemap.server = "Esri.WorldTopoMap")

      cluster_map_result(map)

      incProgress(1)
    })
  })

  output$cluster_map <- renderTmap({
    req(cluster_map_result())
    cluster_map_result()
  })

  output$cluster_map_description <- renderUI({
    HTML(paste0(
      "<div style='color:black;'>",
      "This map shows the geographical distribution of weather stations grouped by their time series similarity. ",
      "Each color represents a distinct cluster of stations with similar rainfall patterns over time. ",
      "<i>To understand the behavior of each cluster, refer to the Clustered Series Plot below.</i>",
      "</div>"
    ))
  })

  output$series_plot <- renderPlot({
    req(cluster_model())
    plot(cluster_model(), type = "series")
  })

  output$series_plot_description <- renderUI({
    HTML(paste0(
      "<div style='color:black;'>",
      "This panel shows the rainfall time series for each station, grouped by cluster. ",
      "Each cluster is displayed in a separate panel, where each colored line represents the rainfall pattern of a station over time.<br><br>",
      "<b>How to interpret:</b><br>",
      "- Clusters with <b>densely packed lines</b> indicate high similarity in temporal behavior among stations.<br>",
      "- Clusters with <b>more spread out lines</b> suggest greater variability within the group.<br>",
      "- Look for seasonal spikes or stable patterns that characterize the nature of each cluster.",
      "</div>"
    ))
  })

  output$metrics <- renderPrint({
    req(cluster_model())
    cvi(cluster_model(), type = "internal")
  })

  output$metrics_description <- renderUI({
    HTML(paste0(
      "<div style='color:black;'>",
      "These metrics assess the quality of the clustering results.<br>",
      "<ul>",
      "<li><b>Sil:</b> Silhouette score - higher values indicate better-defined clusters.</li>",
      "<li><b>CH:</b> Calinski-Harabasz Index - higher is better.</li>",
      "<li><b>DB:</b> Davies-Bouldin Index - lower is better.</li>",
      "<li><b>SF, D, COP, etc.:</b> Additional indices to evaluate compactness and separation.</li>",
      "</ul>",
      "</div>"
    ))
  })

  output$clustering_results <- renderUI({
    req(cluster_map_result())

    div(
      h4(tags$b("Spatial Cluster Map")),
      tmapOutput("cluster_map"),
      br(),
      htmlOutput("cluster_map_description"),

      br(),
      br(),

      h4(tags$b("Clustered Series Plot")),
      plotOutput("series_plot"),
      br(),
      htmlOutput("series_plot_description"),

      br(),
      br(),

      h4(tags$b("Evaluation Metrics")),
      verbatimTextOutput("metrics"),
      htmlOutput("metrics_description")
    )
  })

  # ----------------------------------
  # Decomposition

  observe({
    req(tsibble_data_data())
    tsibble_data <- tsibble_data_data()
    updateSelectInput(inputId = "decomp_station", choices = unique(tsibble_data$Station))
  })

  ts_plot_result <- reactiveVal()
  stl_plot_result <- reactiveVal()

  observeEvent(input$run_decomposition, {
    req(tsibble_data_data())
    tsibble_data <- tsibble_data_data()

    withProgress(message = "Running decomposition...", value = 0, {
      station_name <- input$decomp_station
      req(station_name)

      incProgress(0.3, detail = "Generating time series plot")
      ts_plot <- tsibble_data %>%
        filter(Station == station_name) %>%
        gg_tsdisplay(MonthlyRain) +
        labs(title = paste("Monthly Rainfall in", station_name))
      ts_plot_result(ts_plot)

      incProgress(0.6, detail = "Fitting STL model")
      stl_plot <- tsibble_data %>%
        filter(Station == station_name) %>%
        model(stl = STL(MonthlyRain)) %>%
        components() %>%
        autoplot() +
        labs(title = paste("STL Decomposition for", station_name))
      stl_plot_result(stl_plot)

      incProgress(1)
    })

      output$ts_description <- renderUI({
        HTML(paste0(
          "<div style='color:black;'>",
          "The top panel shows the raw monthly rainfall values for <b>", station_name, "</b> over time.<br>",
          "The ACF (Autocorrelation Function) plot indicates how strongly rainfall values are correlated with past months - helping detect repeating cycles or persistence in rainfall patterns.<br>",
          "The bottom-right plot compares seasonal rainfall trends by month across different years. Each line represents a year, helping identify months with consistently higher or lower rainfall.<br>",
          "<i>Use this visualization to spot general trends, seasonality, and year-to-year variation.</i>",
          "</div>"
        ))
      })

      output$stl_description <- renderUI({
        HTML(paste0(
          "<div style='color:black;'>",
          "This chart breaks the rainfall time series of <b>", station_name, "</b> into three additive components using <b>STL (Seasonal-Trend decomposition using Loess)</b>:<br>",
          "<ul style='margin-top: 0.5rem; margin-bottom: 0.5rem;'>",
          "<li><b>Trend:</b> Long-term direction of rainfall (e.g., increasing, stable, or decreasing over years).</li>",
          "<li><b>Seasonal:</b> Repeating monthly or annual patterns, such as monsoon seasons.</li>",
          "<li><b>Remainder:</b> Irregular fluctuations that aren't explained by trend or seasonality - often noise or anomalies.</li>",
          "</ul>",
          "This decomposition helps you identify meaningful patterns and isolate unusual rainfall behavior at the selected station.",
          "</div>"
        ))
      })
    })

  output$ts_decomp_plot <- renderPlot({
    req(ts_plot_result())
    ts_plot_result()
  })


  output$stl_decomp_plot <- renderPlot({
    req(stl_plot_result())
    stl_plot_result()
  })

  output$decomp_results <- renderUI({
    req(ts_plot_result())
    req(stl_plot_result())

    div(
      h4(tags$b("Time Series Decomposition")),
      plotOutput("ts_decomp_plot"),
      br(),
      htmlOutput("ts_description"),

      br(),
      br(),

      h4(tags$b("STL Decomposition")),
      plotOutput("stl_decomp_plot"),
      br(),
      htmlOutput("stl_description")
    )
  })


  # Forecasting
  forecast_map_result <- reactiveVal()

  observeEvent(input$run_forecast, {
    req(weather_predicted_sf_data())
    weather_predicted_sf <- weather_predicted_sf_data()

    withProgress(message = "Running forecast visualization...", value = 0, {
      selected_date <- as.Date(input$forecast_date)
      req(selected_date)

      forecast_data <- weather_predicted_sf %>%
        filter(Date == selected_date)

      mean_rain <- mean(forecast_data$Daily.Rainfall.Total..mm., na.rm = TRUE)
      sd_rain <- sd(forecast_data$Daily.Rainfall.Total..mm., na.rm = TRUE)

      forecast_data <- forecast_data %>%
        mutate(
          z_score = (Daily.Rainfall.Total..mm. - mean_rain) / sd_rain,
          is_anomaly = abs(z_score) > 2
        )

      forecast_data$is_anomaly <- factor(forecast_data$is_anomaly, levels = c(TRUE, FALSE))

      tmap_mode("view")

      map <- tm_shape(sg_boundary) +
        tm_polygons(col = "grey90", border.col = "white") +
        tm_shape(forecast_data) +
        tm_symbols(
          col = "is_anomaly",
          col.scale = tm_scale(c("#d7191c", "#2c7fb8")),
          size = "Daily.Rainfall.Total..mm.",
          size.scale = tm_scale(1),
          title.size = "Rainfall (mm)",
          col.legend = tm_legend("Anomaly"),
          popup.vars = c("Station", "Daily.Rainfall.Total..mm.")
        ) +
        tm_layout(legend.outside = TRUE, frame = FALSE) +
        tm_title(paste("Rainfall Forecast & Anomalies for", selected_date)) +
        tm_view(basemap.server = "Esri.WorldTopoMap")

      forecast_map_result(map)
      incProgress(1)
    })

      output$forecast_description <- renderUI({
        HTML(paste0(
          "<div style='color:black;'>",
          "This map shows the forecasted rainfall of weather stations for <b>", selected_date, "</b>. ",
          "Each station is represented by a colored circle whose <b>size</b> reflects the predicted rainfall amount (in mm), and <b>color</b> indicates whether the rainfall is considered an anomaly. ",
          "<i>Use this map to identify stations experiencing unusually high rainfall.</i> Larger red circles represent potential <b>extreme rainfall events</b>.",
          "</div>"
        ))
      })
    })


  output$forecast_map <- renderTmap({
    req(forecast_map_result())
    forecast_map_result()
  })

  output$forecasting_results <- renderUI({
    req(forecast_map_result())
    div (
      tmapOutput("forecast_map"),
      br(),
      htmlOutput("forecast_description")
    )

  })
  
}

