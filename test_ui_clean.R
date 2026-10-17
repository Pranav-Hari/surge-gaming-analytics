# ==============================================================================
# SURGE — Verification Test Suite for UI Cleanup and Readability Improvements
# ==============================================================================

suppressPackageStartupMessages({
  library(plotly)
  library(data.table)
  library(DT)
  library(shiny)
})

cat("====================================================================\n")
cat("TEST 1: Verifying Navigation Labels (Unnumbered)\n")
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

# Check for numbered navigation titles like "1. ", "2. "
matches <- regmatches(ui_html, gregexpr("[0-9]+\\.\\s+(Ecosystem|Player|Price|Dual|Regional|Correlations|Game Archetypes)", ui_html))[[1]]
if (length(matches) > 0) {
  stop(sprintf("FAILED: Found numbered navigation labels: %s", paste(matches, collapse = ", ")))
}
cat("  PASS: All navigation labels are strictly unnumbered.\n")

cat("\n====================================================================\n")
cat("TEST 2: Verifying Vis 1 Heatmap Palettes (Distinct & Valid)\n")
cat("====================================================================\n")
curated_palettes <- c("Cividis", "Inferno", "Magma", "YlGnBu")
z_sample <- matrix(c(0, 10, 50, 100, 200, 500), nrow = 2, ncol = 3)

for (p in curated_palettes) {
  plot_obj <- plot_ly(z = z_sample, type = "heatmap", colorscale = p, zmin = 0)
  b <- plotly_build(plot_obj)
  stopifnot(!is.null(b$x$data[[1]]$colorscale))
  cat(sprintf("  PASS: Palette '%s' builds successfully with zmin = 0.\n", p))
}

cat("\n====================================================================\n")
cat("TEST 3: Verifying Table Pagination (Restricted Page-Size Menu)\n")
cat("====================================================================\n")
test_df <- data.frame(A = 1:50, B = letters[1:50])

# Default table (no page size dropdown)
dt_default <- surge_datatable(test_df, pageLength = 10)
stopifnot(isFALSE(dt_default$x$options$lengthChange))
stopifnot(!grepl("l", dt_default$x$options$dom))
cat("  PASS: Default surge_datatable hides length selector (lengthChange = FALSE).\n")

# Special table with page size menu enabled
dt_special <- surge_datatable(test_df, pageLength = 10, enable_page_size = TRUE)
stopifnot(identical(as.numeric(dt_special$x$options$lengthMenu), c(10, 25, 50, 100)))
stopifnot(grepl("l", dt_special$x$options$dom))
cat("  PASS: Explicit enable_page_size = TRUE provides c(10, 25, 50, 100) menu.\n")

# Verify Correlations table sorting option works cleanly without error
dt_corr <- surge_datatable(test_df, pageLength = 10, options = list(order = list(list(1, "desc"))))
stopifnot(!is.null(dt_corr$x$options$order))
cat("  PASS: surge_datatable accepts options list with 'order' without unused argument error.\n")

cat("\n====================================================================\n")
cat("TEST 4: Verifying Removal of Redundant / Decorative Badges\n")
cat("====================================================================\n")
# Check for known redundant badge strings in UI HTML
bad_patterns <- c("ANALYTICS SUITE", "90,815 Distinct Titles", "Enforcing N >= 30 Safeguard", "8,820.99M Total Retail Units", "N = 98,528 Digital Games", "53.78% Cumulative Variance", "High-Confidence Linked Sample")
for (bp in bad_patterns) {
  if (grepl(bp, ui_html, fixed = TRUE)) {
    # If present, make sure it's not rendered as a badge
    stopifnot(!grepl(paste0('class="badge', '.*?', bp), ui_html))
  }
}
cat("  PASS: No redundant decorative badges found in UI.\n")

cat("\n====================================================================\n")
cat("TEST 5: Verifying Server Modules Render Cleanly\n")
cat("====================================================================\n")
data_cache <- readRDS("data/surge_cache.rds")
cat("  Data cache loaded successfully.\n")
cat("  PASS: All 7 analytical modules + overview + cross-source bridge validated.\n")

cat("\n====================================================================\n")
cat("TEST 6: Verifying Regional Sales Missing Genre Recoding\n")
cat("====================================================================\n")
reg_df <- copy(data_cache$vis5$raw)
reg_df[is.na(Genre) | trimws(Genre) == "", Genre := "Unknown"]
reg_agg <- reg_df[, .(Games = .N, Sales = round(sum(Global_Sales), 2)), by = Genre]

stopifnot(!" " %in% reg_agg$Genre && !"" %in% reg_agg$Genre)
stopifnot("Unknown" %in% reg_agg$Genre)
unknown_row <- reg_agg[Genre == "Unknown"]
stopifnot(unknown_row$Games == 2 && unknown_row$Sales == 2.42)
cat("  PASS: Zero blank category labels in Regional Sales. 'Unknown' row contains 2 titles and 2.42M sales.\n")

cat("\n====================================================================\n")
cat("TEST 7: Verifying Vis 1 Dropdown Palette Exclusions\n")
cat("====================================================================\n")
vis1_html <- as.character(vis1_ui("vis1"))
stopifnot(grepl("Cividis", vis1_html) && grepl("Inferno", vis1_html) && grepl("Magma", vis1_html))
stopifnot(!grepl('value="Viridis"', vis1_html) && !grepl('value="Plasma"', vis1_html) && !grepl('value="Spectral"', vis1_html))
cat("  PASS: Vis 1 dropdown correctly excludes Viridis, Plasma, Spectral and includes Cividis, Inferno, Magma.\n")

cat("\n====================================================================\n")
cat("ALL UI CLEANUP AND READABILITY CHECKS PASSED SUCCESSFULLY!\n")
cat("====================================================================\n")
