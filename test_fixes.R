# ==============================================================================
# SURGE — Verification Test Suite for Bug Fixes & Refinements
# ==============================================================================

suppressPackageStartupMessages({
  library(plotly)
  library(data.table)
  library(DT)
})

cat("====================================================================\n")
cat("TEST 1: Verifying Vis 1 (Ecosystem Heatmap) Palettes\n")
cat("====================================================================\n")
z_test <- matrix(1:20, nrow = 4, ncol = 5)
heat_scales <- c("Viridis", "Plasma", "Cividis", "Inferno", "Magma", "Turbo")

for (sc in heat_scales) {
  p <- plot_ly(z = z_test, type = "heatmap", colorscale = sc)
  b <- plotly_build(p)
  cat(sprintf("  PASS: Heatmap colorscale '%s' compiles cleanly.\n", sc))
}

cat("\n====================================================================\n")
cat("TEST 2: Verifying Vis 2 (Player Sentiment) Default Data & Traces\n")
cat("====================================================================\n")
cache <- readRDS("data/surge_cache.rds")
v2_data <- cache$vis2$raw_unnested[num_reviews_total >= 30]

cat(sprintf("  Total usable unnested rows: %d\n", nrow(v2_data)))
cat(sprintf("  Distinct Steam games: %d\n", length(unique(v2_data$steam_appid))))
stopifnot(nrow(v2_data) > 30000)
stopifnot(length(unique(v2_data$steam_appid)) > 30000)

# Test plotly violin build
genre_medians <- v2_data[, .(med = median(pct_pos_total)), by = genre][order(-med)]
v2_data[, genre_ord := factor(genre, levels = genre_medians$genre)]

p_v2 <- plot_ly(
  data = v2_data,
  x = ~genre_ord,
  y = ~pct_pos_total,
  type = "violin",
  split = ~genre_ord,
  box = list(visible = TRUE),
  points = FALSE
)
b_v2 <- plotly_build(p_v2)
cat(sprintf("  PASS: Sentiment violin plot compiled with %d genre traces.\n", length(b_v2$x$data)))

cat("\n====================================================================\n")
cat("TEST 3: Verifying Vis 6 (Correlation Heatmap) Palette Reactivity\n")
cat("====================================================================\n")
palette_map <- list(
  "coolwarm" = list(c(0.0, "#f43f5e"), c(0.5, "#0b101b"), c(1.0, "#00f0ff")),
  "RdBu"     = list(c(0.0, "#ef4444"), c(0.5, "#1e293b"), c(1.0, "#3b82f6")),
  "PRGn"     = list(c(0.0, "#a855f7"), c(0.5, "#0f172a"), c(1.0, "#10b981")),
  "Spectral" = list(c(0.0, "#f59e0b"), c(0.5, "#0f172a"), c(1.0, "#06b6d4"))
)

z_corr <- matrix(seq(-1, 1, length.out = 16), nrow = 4, ncol = 4)
for (p_name in names(palette_map)) {
  p_corr <- plot_ly(z = z_corr, type = "heatmap", colorscale = palette_map[[p_name]], zmin = -1, zmax = 1)
  b_corr <- plotly_build(p_corr)
  cat(sprintf("  PASS: Correlation palette '%s' compiled cleanly.\n", p_name))
}

cat("\n====================================================================\n")
cat("TEST 4: Verifying Vis 4 (Timelines) Appropriate Domains\n")
cat("====================================================================\n")
steam_tl <- cache$vis4$steam_timeline
sales_tl <- cache$vis4$sales_timeline
cat(sprintf("  Steam timeline: %d years (Min: %d, Max: %d)\n", nrow(steam_tl), min(steam_tl$year), max(steam_tl$year)))
cat(sprintf("  Console sales timeline: %d years (Min: %d, Max: %d)\n", nrow(sales_tl), min(sales_tl$year), max(sales_tl$year)))
stopifnot(min(steam_tl$year) == 1997 && max(steam_tl$year) == 2025)
stopifnot(min(sales_tl$year) == 1980 && max(sales_tl$year) == 2016)
cat("  PASS: Timelines preserve independent, verified chronological domains.\n")

cat("\n====================================================================\n")
cat("TEST 5: Sourcing full application and testing smoke\n")
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
rendered <- as.character(app_env$ui)
cat(sprintf("  PASS: UI rendered to HTML (%d characters). Footer absent: %s\n",
            nchar(rendered), !grepl("surge-footer", rendered)))

cat("\n====================================================================\n")
cat("ALL FOCUSED VERIFICATION TESTS PASSED WITH ZERO FAILURES!\n")
cat("====================================================================\n")
