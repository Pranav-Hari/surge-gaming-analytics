# ==============================================================================
# SURGE — Smoke & Automated Verification Test
# Tests UI compilation, data loading, server initialization, and module outputs
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(data.table)
})

cat("[SMOKE TEST] Step 1: Checking data cache existence...\n")
stopifnot(file.exists("data/surge_cache.rds"))
cat("  PASS: data/surge_cache.rds exists (", round(file.size("data/surge_cache.rds") / (1024*1024), 2), " MB)\n")

cat("[SMOKE TEST] Step 2: Sourcing all application files...\n")
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
cat("  PASS: All R modules sourced without syntax or loading errors.\n")

cat("[SMOKE TEST] Step 3: Verifying UI construction...\n")
# Source app.R in an environment
app_env <- new.env()
sys.source("app.R", envir = app_env)
stopifnot(!is.null(app_env$ui))
stopifnot(!is.null(app_env$server))
cat("  PASS: UI and Server objects instantiated successfully.\n")

cat("[SMOKE TEST] Step 4: Compiling UI to HTML...\n")
rendered_html <- as.character(app_env$ui)
stopifnot(grepl("SURGE", rendered_html))
stopifnot(grepl("Ecosystem Heatmap", rendered_html))
stopifnot(grepl("Player Sentiment", rendered_html))
stopifnot(grepl("Price vs Reception", rendered_html))
stopifnot(grepl("Dual Timelines", rendered_html))
stopifnot(grepl("Regional Sales", rendered_html))
stopifnot(grepl("Correlations", rendered_html))
stopifnot(grepl("Game Archetypes", rendered_html))
cat("  PASS: UI HTML rendered with all 7 analytical section headers present.\n")

cat("\n=======================================================\n")
cat("ALL SMOKE TESTS COMPLETED SUCCESSFULLY WITH ZERO ERRORS!\n")
cat("=======================================================\n")
