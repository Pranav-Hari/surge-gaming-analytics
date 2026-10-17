# ==============================================================================
# SURGE — Regal Branding (Option C) Automated Verification Test
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(data.table)
})

cat("====================================================================\n")
cat("TEST 1: Sourcing All Modules and Checking UI Compilation\n")
cat("====================================================================\n")
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

app_env <- new.env()
sys.source("app.R", envir = app_env)
ui_html <- as.character(app_env$ui)
stopifnot(!is.null(app_env$ui))
stopifnot(!is.null(app_env$server))
cat("  PASS: All R modules compiled and UI instantiated.\n")

cat("\n====================================================================\n")
cat("TEST 2: Verifying Option C Brand Colors in R/theme.R\n")
cat("====================================================================\n")
stopifnot(identical(SURGE_PALETTE$bg_primary, "#08090D"))
stopifnot(identical(SURGE_PALETTE$gold, "#C9A227"))
stopifnot(identical(SURGE_PALETTE$velvet, "#4B236A"))
stopifnot(identical(SURGE_PALETTE$crimson, "#9B111E"))
stopifnot(identical(SURGE_PALETTE$royal_blue, "#4169E1"))
cat("  PASS: SURGE_PALETTE accurately defines all Option C colours:\n")
cat(sprintf("        Rich Black:         %s\n", SURGE_PALETTE$bg_primary))
cat(sprintf("        Royal Antique Gold: %s\n", SURGE_PALETTE$gold))
cat(sprintf("        Royal Velvet:       %s\n", SURGE_PALETTE$velvet))
cat(sprintf("        Imperial Crimson:   %s\n", SURGE_PALETTE$crimson))
cat(sprintf("        Royal Blue:         %s\n", SURGE_PALETTE$royal_blue))

cat("\n====================================================================\n")
cat("TEST 3: Verifying Option C Brand Colors and Font in www/styles.css\n")
cat("====================================================================\n")
css_content <- paste(readLines("www/styles.css"), collapse = "\n")
stopifnot(grepl("Libre\\+Barcode\\+39\\+Extended\\+Text", css_content))
stopifnot(grepl("#08090D", css_content, ignore.case = TRUE))
stopifnot(grepl("#C9A227", css_content, ignore.case = TRUE))
stopifnot(grepl("#4B236A", css_content, ignore.case = TRUE))
stopifnot(grepl("#9B111E", css_content, ignore.case = TRUE))
stopifnot(grepl("#4169E1", css_content, ignore.case = TRUE))
stopifnot(grepl("brand-wordmark", css_content))
cat("  PASS: www/styles.css imports Libre Barcode 39 Extended Text and defines Option C variables.\n")

cat("\n====================================================================\n")
cat("TEST 4: Verifying Wordmark Rendering in UI HTML\n")
cat("====================================================================\n")
stopifnot(grepl('class="brand-wordmark"', ui_html))
stopifnot(grepl('>SURGE<', ui_html))
cat("  PASS: SURGE wordmark is rendered in navbar with class 'brand-wordmark'.\n")

cat("\n====================================================================\n")
cat("TEST 5: CRITICAL CHECK — Verifying Analytical Palettes Preserved\n")
cat("====================================================================\n")
# Vis 1: Heatmap palettes
v1_code <- paste(readLines("R/mod_vis1_ecosystem.R"), collapse = "\n")
stopifnot(grepl("Cividis", v1_code))
stopifnot(grepl("Inferno", v1_code))
stopifnot(grepl("Magma", v1_code))
stopifnot(grepl("YlGnBu", v1_code))

# Vis 5: Regional sales colors
v5_code <- paste(readLines("R/mod_vis5_regional.R"), collapse = "\n")
stopifnot(grepl("#38bdf8", v5_code)) # NA
stopifnot(grepl("#f59e0b", v5_code)) # EU
stopifnot(grepl("#f43f5e", v5_code)) # JP
stopifnot(grepl("#64748b", v5_code)) # Other

# Vis 7: Cluster colors
v7_code <- paste(readLines("R/mod_vis7_archetypes.R"), collapse = "\n")
stopifnot(grepl("#00f0ff", v7_code)) # Commercial
stopifnot(grepl("#10b981", v7_code)) # Mid-tier
stopifnot(grepl("#a855f7", v7_code)) # Modest
stopifnot(grepl("#64748b", v7_code)) # Barebones
stopifnot(grepl("#f59e0b", v7_code)) # Star highlight

cat("  PASS: All analytical and chart palettes strictly preserved without alteration.\n")

cat("\n====================================================================\n")
cat("ALL REGAL BRANDING (OPTION C) CHECKS PASSED WITH ZERO ERRORS!\n")
cat("====================================================================\n")
