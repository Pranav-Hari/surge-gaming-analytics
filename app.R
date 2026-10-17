# ==============================================================================
# SURGE — An Interactive Visual Analytics Platform for the Gaming Industry
# CSI3005: Advanced Data Visualization Techniques
# Application Entry Point
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(data.table)

# 1. Source System Modules
source("R/theme.R")
source("R/data_loader.R")
source("R/mod_overview.R")
source("R/mod_vis1_ecosystem.R")
source("R/mod_vis2_sentiment.R")
source("R/mod_vis3_pricing.R")
source("R/mod_vis4_timelines.R")
source("R/mod_vis5_regional.R")
source("R/mod_vis6_correlation.R")
source("R/mod_vis7_archetypes.R")
source("R/mod_cross_drilldown.R")

# 2. Ingest Verified Datasets into Global Cache
SURGE_DATA <- get_surge_data("data/surge_cache.rds")

# 3. Application User Interface Definition
ui <- page_navbar(
  theme = surge_bs_theme(),
  title = tags$a(
    id = "brand_home_link",
    href = "#",
    class = "brand-nav-link",
    role = "button",
    tabindex = "0",
    `aria-label` = "SURGE Overview - Return to dashboard overview",
    onclick = "if (window.Shiny) { Shiny.setInputValue('nav_to_overview', Math.random()); } return false;",
    span(class = "brand-wordmark", "SURGE")
  ),
  id = "main_nav",
  window_title = "SURGE — Gaming Industry Exposed",
  header = tagList(
    tags$head(
      tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Libre+Barcode+39+Extended+Text&family=Inter:wght@300;400;500;600;700&family=Outfit:wght@400;600;700;800;900&family=JetBrains+Mono:wght@400;500;700&display=swap"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=wordmark_fix"),
      tags$script(HTML("
        $(document).on('keydown', '#brand_home_link', function(e) {
          if (e.key === 'Enter' || e.key === ' ') {
            e.preventDefault();
            $(this).click();
          }
        });
      ")),
      tags$meta(name = "description", content = "SURGE: Advanced interactive visual analytics platform exposing multi-platform gaming industry dynamics, Spearman correlations, and PCA clustering.")
    )
  ),

  # Navigation Tabs
  nav_panel(
    title = "Overview",
    overview_ui("overview")
  ),
  nav_panel(
    title = "Ecosystem Heatmap",
    vis1_ui("vis1")
  ),
  nav_panel(
    title = "Player Sentiment",
    vis2_ui("vis2")
  ),
  nav_panel(
    title = "Price vs Reception",
    vis3_ui("vis3")
  ),
  nav_panel(
    title = "Dual Timelines",
    vis4_ui("vis4")
  ),
  nav_panel(
    title = "Regional Sales",
    vis5_ui("vis5")
  ),
  nav_panel(
    title = "Correlations",
    vis6_ui("vis6")
  ),
  nav_panel(
    title = "Game Archetypes",
    vis7_ui("vis7")
  ),
  nav_panel(
    title = "Cross-Source Bridge",
    cross_drilldown_ui("cross_drill")
  )
)

# 4. Application Server Orchestration
server <- function(input, output, session) {
  # Direct navigation to Overview when clicking brand logo
  observeEvent(input$nav_to_overview, {
    nav_select("main_nav", "Overview")
  })

  # Server Module Activations
  overview_server("overview", SURGE_DATA)
  vis1_server("vis1", SURGE_DATA)
  vis2_server("vis2", SURGE_DATA)
  vis3_server("vis3", SURGE_DATA)
  vis4_server("vis4", SURGE_DATA)
  vis5_server("vis5", SURGE_DATA)
  vis6_server("vis6", SURGE_DATA)
  vis7_server("vis7", SURGE_DATA)
  cross_drilldown_server("cross_drill", SURGE_DATA)
}

# 5. Launch Executable
shinyApp(ui = ui, server = server)
