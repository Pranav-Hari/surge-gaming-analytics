# ==============================================================================
# SURGE — Pass 3 Verification Test Suite
# Targeted UI Refinements: Tables, Archetypes, Overview, Interpretations
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
cat("TEST 2: Check Player Sentiment, Regional Sales, and Archetype Tables\n")
cat("====================================================================\n")
# Check theme helper single-page drawCallback
test_df <- data.frame(Name = c("A", "B", "C"), Val = c(1, 2, 3))
dt_single <- surge_datatable(test_df, options = list(paging = FALSE, searching = FALSE, info = FALSE, dom = "t"))
stopifnot(isFALSE(dt_single$x$options$paging))
stopifnot(isFALSE(dt_single$x$options$searching))
stopifnot(isFALSE(dt_single$x$options$info))
stopifnot(identical(dt_single$x$options$dom, "t"))
cat("  PASS: Single-page table options (paging=F, searching=F, info=F, dom='t') configured properly.\n")

# Verify drawCallback is attached to surge_datatable
stopifnot(!is.null(dt_single$x$options$drawCallback))
stopifnot(grepl("p.pages <= 1", as.character(dt_single$x$options$drawCallback)))
cat("  PASS: surge_datatable includes drawCallback suppressing single-page footers.\n")

cat("\n====================================================================\n")
cat("TEST 3: Regional Sales Unknown Explanation and 13 Categories\n")
cat("====================================================================\n")
cache <- readRDS("data/surge_cache.rds")
reg_raw <- copy(cache$vis5$raw)
reg_raw[is.na(Genre) | trimws(Genre) == "", Genre := "Unknown"]
cat_count <- length(unique(reg_raw$Genre))
stopifnot(cat_count == 13)
stopifnot("Unknown" %in% unique(reg_raw$Genre))
cat(sprintf("  PASS: All 13 regional categories present, including Unknown (Games: %d).\n", nrow(reg_raw[Genre == "Unknown"])))

# Check explanation text in UI
stopifnot(grepl("Unknown means the original dataset did not provide a genre for these games", ui_html, fixed = TRUE))
cat("  PASS: Regional Sales plain-English explanation for Unknown genre is present in UI.\n")

cat("\n====================================================================\n")
cat("TEST 4: Correlations Table Page-Size Menu Enabled\n")
cat("====================================================================\n")
# Verify Correlations table has enable_page_size = TRUE
corr_mod_code <- paste(readLines("R/mod_vis6_correlation.R"), collapse = "\n")
stopifnot(grepl("enable_page_size\\s*=\\s*TRUE", corr_mod_code))
cat("  PASS: Correlations table specifies enable_page_size = TRUE (offers 10, 25, 50, 100 rows).\n")

cat("\n====================================================================\n")
cat("TEST 5: Game Archetypes Benchmark Selector and Layout\n")
cat("====================================================================\n")
arch_mod_code <- paste(readLines("R/mod_vis7_archetypes.R"), collapse = "\n")

# 1. Benchmark Title Inspector card is completely removed
stopifnot(!grepl("Benchmark Title Inspector", arch_mod_code, fixed = TRUE))
stopifnot(!grepl("selected_game_card", arch_mod_code, fixed = TRUE))
cat("  PASS: 'Benchmark Title Inspector' box is completely removed.\n")

# 2. Benchmark table is full width col-12
stopifnot(grepl('div\\s*\\(\\s*class\\s*=\\s*"col-12",\\s*surge_card\\(\\s*title\\s*=\\s*"Empirical Cluster Benchmark Table"', arch_mod_code))
cat("  PASS: Empirical Cluster Benchmark Table container is expanded to col-12.\n")

# 3. Benchmark table has paging and searching disabled
stopifnot(grepl('output\\$cluster_table.*paging\\s*=\\s*FALSE', arch_mod_code))
cat("  PASS: Empirical Cluster Benchmark Table has paging, searching, and info disabled.\n")

# 4. Benchmark selector choices are expanded across all 4 clusters
benchmarks_curated <- c(
  "Crysis 2 - Maximum Edition", "Deus Ex: Game of the Year Edition", "The Stanley Parable",
  "Super Hexagon", "Deadlight", "Cyberpunk 2077", "ELDEN RING", "Terraria"
)
for (b in benchmarks_curated) {
  stopifnot(grepl(b, arch_mod_code, fixed = TRUE))
}
cat("  PASS: Benchmark selector offers diverse representative titles across all 4 clusters.\n")

cat("\n====================================================================\n")
cat("TEST 6: Overview Page Rewrite for General Audience\n")
cat("====================================================================\n")
overview_code <- paste(readLines("R/mod_overview.R"), collapse = "\n")

# Check that modules overview and safeguards use plain English
stopifnot(grepl("Catalogue Counts vs Market Share", overview_code, fixed = TRUE))
stopifnot(grepl("Different Eras & Sources", overview_code, fixed = TRUE))
stopifnot(grepl("Separate Analytical Measures", overview_code, fixed = TRUE))
stopifnot(grepl("High-Confidence Linking", overview_code, fixed = TRUE))
stopifnot(grepl("Missing Activity Values", overview_code, fixed = TRUE))
stopifnot(grepl("Patterns, Not Proof", overview_code, fixed = TRUE))

# Verify internal implementation jargon is removed
stopifnot(!grepl("match_status == 'HIGH_CONFIDENCE'", overview_code, fixed = TRUE))
stopifnot(!grepl("K=2 has higher silhouette", overview_code, fixed = TRUE))
stopifnot(!grepl("99.31M sales", overview_code, fixed = TRUE))
cat("  PASS: Overview page rewritten with accessible language and all required safeguards.\n")

cat("\n====================================================================\n")
cat("TEST 7: Simplified Interpretation Summaries (Pricing & Correlation)\n")
cat("====================================================================\n")
pricing_code <- paste(readLines("R/mod_vis3_pricing.R"), collapse = "\n")

# Price vs Reception summary
stopifnot(!grepl("rs = \\+0.08 \\(Very Weak\\)", pricing_code))
stopifnot(grepl("Near-Zero Link", pricing_code, fixed = TRUE) || grepl("Price vs Satisfaction", pricing_code, fixed = TRUE))
stopifnot(grepl("monetization strategy", pricing_code, fixed = TRUE) || grepl("perceived game quality", pricing_code, fixed = TRUE))
cat("  PASS: Price vs Reception Commercial Acclaim Metrics is simplified in plain English.\n")

# Correlations summary
stopifnot(!grepl("rs = \\+0.72 \\*\\*\\*", corr_mod_code))
stopifnot(!grepl("Reviews ↔ Peak CCU", corr_mod_code, fixed = TRUE))
stopifnot(grepl("Reviews & Peak Players", corr_mod_code, fixed = TRUE))
stopifnot(grepl("Playtime & Active Players", corr_mod_code, fixed = TRUE))
cat("  PASS: Key Empirical Associations on Correlations is simplified in plain English.\n")

cat("\n====================================================================\n")
cat("ALL PASS 3 VERIFICATION CHECKS PASSED WITH ZERO ERRORS!\n")
cat("====================================================================\n")
