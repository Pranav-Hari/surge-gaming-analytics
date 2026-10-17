# ==============================================================================
# SURGE — Application Launcher Script
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
})

options(shiny.port = 3838, shiny.host = "127.0.0.1")
shiny::runApp(".", launch.browser = TRUE)
