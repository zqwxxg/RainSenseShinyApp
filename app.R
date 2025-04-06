library(shiny)

# Source UI and server scripts
source("ui.R")
source("server.R", local = TRUE)

shinyApp(ui = ui, server = server)