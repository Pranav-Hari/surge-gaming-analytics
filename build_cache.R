# ==============================================================================
# SURGE — Precomputation and Caching Script
# Executes all required statistical transformations and saves to data/surge_cache.rds
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
})

cat("[SURGE BUILD] Starting analytical pipeline precomputation...\n")

if (!dir.exists("data")) dir.create("data")

# 1. Load Datasets
cat("[SURGE BUILD] Loading raw CSV files...\n")
steam_raw <- fread("Datasets/Processed/steam_games_clean.csv")
igdb_raw  <- fread("Datasets/Processed/multiplatform_igdb_hltb.csv")
plat_raw  <- fread("Datasets/Processed/game_platforms.csv")
hist_raw  <- fread("Datasets/Processed/historical_console_sales.csv")
bridge_raw <- fread("Datasets/Processed/cross_platform_bridge.csv")

cat(sprintf("  Steam: %d rows\n", nrow(steam_raw)))
cat(sprintf("  IGDB: %d rows\n", nrow(igdb_raw)))
cat(sprintf("  Platforms: %d rows\n", nrow(plat_raw)))
cat(sprintf("  Historical Sales: %d rows\n", nrow(hist_raw)))
cat(sprintf("  Bridge: %d rows\n", nrow(bridge_raw)))

# Metadata
meta <- list(
  steam_total_count = nrow(steam_raw),
  igdb_total_count = nrow(igdb_raw),
  platforms_total_count = nrow(plat_raw),
  hist_total_count = nrow(hist_raw),
  hist_clean_count = sum(hist_raw$flag_suspicious_year == FALSE | hist_raw$flag_suspicious_year == "False"),
  hist_clean_sales = sum(hist_raw[flag_suspicious_year == FALSE | flag_suspicious_year == "False"]$Global_Sales),
  hist_excluded_count = sum(hist_raw$flag_suspicious_year == TRUE | hist_raw$flag_suspicious_year == "True"),
  hist_excluded_sales = sum(hist_raw[flag_suspicious_year == TRUE | flag_suspicious_year == "True"]$Global_Sales),
  bridge_total_count = nrow(bridge_raw),
  bridge_high_conf = sum(bridge_raw$match_status == "HIGH_CONFIDENCE")
)

# ------------------------------------------------------------------------------
# Vis 1: Genre x Platform Catalogue
# ------------------------------------------------------------------------------
cat("[SURGE BUILD] Building Vis 1 (Genre x Platform)...\n")
igdb_v1 <- igdb_raw[genre_names != "" & !is.na(genre_names), .(igdb_id, game_name, genre_names, rating, review_score, main)]
plat_v1 <- plat_raw[platform_family != "" & !is.na(platform_family), .(igdb_id, platform_family)]

joined_v1 <- merge(igdb_v1, plat_v1, by = "igdb_id", allow.cartesian = TRUE)

# Unnest genres
v1_pairs <- joined_v1[, .(genre = trimws(unlist(strsplit(genre_names, ",")))), by = .(igdb_id, game_name, platform_family, rating, review_score, main)]
v1_pairs <- v1_pairs[genre != ""]

# Aggregations by distinct igdb_id
v1_agg <- v1_pairs[, .(game_count = uniqueN(igdb_id)), by = .(genre, platform_family)]

# Total distinct games by genre and by platform
v1_genre_totals <- v1_pairs[, .(total_games = uniqueN(igdb_id)), by = genre][order(-total_games)]
v1_plat_totals  <- v1_pairs[, .(total_games = uniqueN(igdb_id)), by = platform_family][order(-total_games)]

# Top 3 titles per cell (by review_score or rating or main)
v1_top_titles <- v1_pairs[, {
  sub <- .SD[order(-coalesce(review_score, 0), -coalesce(rating, 0))]
  top3 <- head(unique(sub$game_name), 3)
  .(top_titles = paste(top3, collapse = ", "))
}, by = .(genre, platform_family)]

v1_agg <- merge(v1_agg, v1_top_titles, by = c("genre", "platform_family"), all.x = TRUE)

# Platform catalogue percentage
v1_agg <- merge(v1_agg, v1_plat_totals[, .(platform_family, plat_total = total_games)], by = "platform_family")
v1_agg[, pct_of_platform := round((game_count / plat_total) * 100, 2)]

vis1_data <- list(
  agg = v1_agg,
  genre_order = v1_genre_totals$genre,
  platform_order = c("PC", "PlayStation", "Nintendo", "Xbox", "Other / Retro"),
  genre_totals = v1_genre_totals,
  plat_totals = v1_plat_totals,
  total_distinct_games = uniqueN(v1_pairs$igdb_id),
  total_pairs = nrow(v1_pairs),
  sample_lookup = v1_pairs[sample(.N, min(20000, .N)), .(game_name, genre, platform_family, rating = round(rating, 1), main_hours = round(main, 1))]
)

# ------------------------------------------------------------------------------
# Vis 2: Steam Sentiment Distribution by Genre
# ------------------------------------------------------------------------------
cat("[SURGE BUILD] Building Vis 2 (Sentiment Distribution)...\n")
steam_v2_cand <- steam_raw[!is.na(pct_pos_total) & num_reviews_total >= 30, .(
  steam_appid, name, genres, pct_pos_total, num_reviews_total, price, is_free, review_score_desc
)]

# Primary top genres to track
target_genres <- c("Action", "Adventure", "RPG", "Strategy", "Simulation", "Casual", "Indie", "Massively Multiplayer", "Racing", "Sports")

# Clean Python list formatting (brackets, quotes) and unnest
steam_v2_cand[, clean_genres := gsub("\\[|\\]|'|\"", "", genres)]
v2_unnested <- steam_v2_cand[, .(genre = trimws(unlist(strsplit(clean_genres, ",")))), by = .(steam_appid, name, pct_pos_total, num_reviews_total, price, is_free, review_score_desc)]
v2_unnested <- v2_unnested[genre %in% target_genres]

# Genre summary statistics
v2_summary <- v2_unnested[, .(
  n_games = .N,
  median_pos = median(pct_pos_total),
  q25 = quantile(pct_pos_total, 0.25),
  q75 = quantile(pct_pos_total, 0.75),
  mean_pos = round(mean(pct_pos_total), 2)
), by = genre][order(-median_pos)]

vis2_data <- list(
  raw_unnested = v2_unnested,
  genre_summary = v2_summary,
  catalog_median = 82.0,
  total_usable_games = uniqueN(steam_v2_cand$steam_appid)
)

# ------------------------------------------------------------------------------
# Vis 3: Steam Pricing vs Reception
# ------------------------------------------------------------------------------
cat("[SURGE BUILD] Building Vis 3 (Price vs Sentiment)...\n")
steam_v3 <- steam_raw[!is.na(pct_pos_total) & num_reviews_total >= 30, .(
  steam_appid, name, price, discount, pct_pos_total, num_reviews_total, is_free, review_score_desc,
  log_reviews = log10(num_reviews_total + 1.0)
)]

# Representative sample for lightning-fast bubble scatter (e.g. 5,000 games) + all high-review titles
high_rev <- steam_v3[num_reviews_total >= 5000]
low_rev  <- steam_v3[num_reviews_total < 5000]
set.seed(42)
sampled_low <- low_rev[sample(.N, min(4000, .N))]
v3_display <- rbind(high_rev, sampled_low)[order(-num_reviews_total)]

vis3_data <- list(
  full_data = steam_v3,
  display_sample = v3_display,
  total_games = nrow(steam_v3),
  paid_count = sum(steam_v3$price > 0),
  free_count = sum(steam_v3$price == 0)
)

# ------------------------------------------------------------------------------
# Vis 4: Coordinated Timelines
# ------------------------------------------------------------------------------
cat("[SURGE BUILD] Building Vis 4 (Timelines)...\n")
# Panel A: Steam Releases 1997-2025
steam_timeline <- steam_raw[release_year >= 1997 & release_year <= 2025, .(
  annual_releases = .N
), by = .(year = release_year)][order(year)]

# Panel B: Historical Console Retail Sales 1980-2016
hist_clean <- hist_raw[flag_suspicious_year == FALSE | flag_suspicious_year == "False"]
hist_clean[, Year_int := as.integer(as.character(Year_of_Release))]
hist_clean_filtered <- hist_clean[Year_int >= 1980 & Year_int <= 2016]

sales_timeline <- hist_clean_filtered[, .(
  retail_sales_millions = round(sum(Global_Sales), 2),
  release_count = .N,
  top_game = {
    top_row <- .SD[order(-Global_Sales)][1]
    paste0(top_row$Name, " (", top_row$Platform, ", ", round(top_row$Global_Sales, 1), "M)")
  }
), by = .(year = Year_int)][order(year)]

vis4_data <- list(
  steam_timeline = steam_timeline,
  sales_timeline = sales_timeline,
  steam_total = nrow(steam_raw),
  sales_clean_total = sum(sales_timeline$retail_sales_millions),
  sales_clean_records = sum(sales_timeline$release_count),
  peak_year = sales_timeline[order(-retail_sales_millions)][1]
)

# ------------------------------------------------------------------------------
# Vis 5: Regional Console Sales Breakdown
# ------------------------------------------------------------------------------
cat("[SURGE BUILD] Building Vis 5 (Regional Sales)...\n")
# Platform family classification
assign_plat_family <- function(plat) {
  p <- toupper(plat)
  fifelse(p %in% c("NES", "SNES", "N64", "GC", "WII", "WIIU", "GB", "GBA", "DS", "3DS"), "Nintendo",
  fifelse(p %in% c("PS", "PS2", "PS3", "PS4", "PSP", "PSV"), "Sony PlayStation",
  fifelse(p %in% c("XB", "X360", "XONE"), "Microsoft Xbox",
  fifelse(p == "PC", "PC", "Sega / Retro / Other"))))
}

hist_clean[, Platform_Family := assign_plat_family(Platform)]

# Add Era
hist_clean[, Era := fifelse(Year_int <= 1995, "1980–1995 (8/16-Bit Era)",
                    fifelse(Year_int <= 2005, "1996–2005 (32/128-Bit Era)", "2006–2016 (HD Era)"))]

# Regional by Genre
reg_genre <- hist_clean[, .(
  NA_Sales = round(sum(NA_Sales), 2),
  EU_Sales = round(sum(EU_Sales), 2),
  JP_Sales = round(sum(JP_Sales), 2),
  Other_Sales = round(sum(Other_Sales), 2),
  Global_Sales = round(sum(Global_Sales), 2),
  Game_Count = .N
), by = Genre][order(-Global_Sales)]

# Regional by Platform Family
reg_plat <- hist_clean[, .(
  NA_Sales = round(sum(NA_Sales), 2),
  EU_Sales = round(sum(EU_Sales), 2),
  JP_Sales = round(sum(JP_Sales), 2),
  Other_Sales = round(sum(Other_Sales), 2),
  Global_Sales = round(sum(Global_Sales), 2),
  Game_Count = .N
), by = Platform_Family][order(-Global_Sales)]

vis5_data <- list(
  raw = hist_clean,
  reg_genre = reg_genre,
  reg_plat = reg_plat,
  total_sales = sum(hist_clean$Global_Sales),
  total_records = nrow(hist_clean)
)

# ------------------------------------------------------------------------------
# Vis 6: Spearman Rank Correlation Matrix
# ------------------------------------------------------------------------------
cat("[SURGE BUILD] Building Vis 6 (Spearman Correlation)...\n")
v6_cols <- c("price", "pct_pos_total", "num_reviews_total", "peak_ccu",
             "average_playtime_forever", "discount", "achievements", "dlc_count")
steam_v6 <- na.omit(steam_raw[, c(..v6_cols, "genres")])

# Feature readable names
v6_labels <- c(
  price = "Base Price ($)",
  pct_pos_total = "Positive Sentiment (%)",
  num_reviews_total = "Total Reviews",
  peak_ccu = "Peak Concurrent Players",
  average_playtime_forever = "Playtime (mins)",
  discount = "Discount Depth (%)",
  achievements = "Achievement Count",
  dlc_count = "DLC Expansions"
)

# Function to compute spearman matrix and p-values
compute_corr <- function(df, cols) {
  mat <- as.matrix(df[, ..cols])
  n <- ncol(mat)
  cor_mat <- matrix(NA, n, n)
  p_mat   <- matrix(NA, n, n)
  colnames(cor_mat) <- cols
  rownames(cor_mat) <- cols
  colnames(p_mat) <- cols
  rownames(p_mat) <- cols

  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        cor_mat[i, j] <- 1.0
        p_mat[i, j] <- 0.0
      } else if (i < j) {
        tst <- suppressWarnings(cor.test(mat[, i], mat[, j], method = "spearman", exact = FALSE))
        cor_mat[i, j] <- cor_mat[j, i] <- round(unname(tst$estimate), 3)
        p_mat[i, j]   <- p_mat[j, i]   <- tst$p.value
      }
    }
  }

  # Hierarchical clustering order
  dist_mat <- as.dist((1 - cor_mat) / 2)
  hc <- hclust(dist_mat)
  order_idx <- hc$order

  list(
    cor = cor_mat[order_idx, order_idx],
    p = p_mat[order_idx, order_idx],
    cols_ordered = cols[order_idx]
  )
}

corr_full <- compute_corr(steam_v6, v6_cols)

# Also precompute for top 4 genres
genres_for_v6 <- c("Action", "RPG", "Strategy", "Indie")
genre_corrs <- list()
for (g in genres_for_v6) {
  sub <- steam_v6[grepl(g, genres, fixed = TRUE)]
  if (nrow(sub) > 100) {
    genre_corrs[[g]] <- compute_corr(sub, v6_cols)
    genre_corrs[[g]]$N <- nrow(sub)
  }
}

vis6_data <- list(
  corr_full = corr_full,
  genre_corrs = genre_corrs,
  v6_labels = v6_labels,
  N_complete = nrow(steam_v6),
  cols = v6_cols
)

# ------------------------------------------------------------------------------
# Vis 7: PCA and K-Means Clustering
# ------------------------------------------------------------------------------
cat("[SURGE BUILD] Building Vis 7 (PCA & K-Means Archetypes)...\n")
v7_candidates <- steam_raw[!is.na(pct_pos_total) & num_reviews_total >= 30, .(
  steam_appid, name, developers, genres, price, pct_pos_total, num_reviews_total, peak_ccu,
  average_playtime_forever, achievements, dlc_count
)]

# Feature Matrix Transformations
v7_features <- data.frame(
  price = v7_candidates$price,
  pct_pos_total = v7_candidates$pct_pos_total,
  log_reviews = log10(v7_candidates$num_reviews_total + 1.0),
  log_ccu = log10(v7_candidates$peak_ccu + 1.0),
  log_playtime = log10(v7_candidates$average_playtime_forever + 1.0),
  log_achievements = log10(v7_candidates$achievements + 1.0),
  log_dlc = log10(v7_candidates$dlc_count + 1.0)
)

v7_scaled <- scale(v7_features)

# PCA
pca_model <- prcomp(v7_scaled, center = FALSE, scale. = FALSE)
pca_var <- pca_model$sdev^2
pca_ve <- round((pca_var / sum(pca_var)) * 100, 2)

# Loadings
pca_loadings <- as.data.frame(pca_model$rotation[, 1:4])
rownames(pca_loadings) <- c("Price", "% Positive", "Log Reviews", "Log CCU", "Log Playtime", "Log Achievements", "Log DLC")

# K-Means (centers = 4, seed 42)
set.seed(42)
km_model <- kmeans(v7_scaled, centers = 4, nstart = 25, iter.max = 50)

# Map clusters to descriptive names according to empirical profiles
# Profile 1: High engagement / Established Commercial (High reviews, High CCU, High price)
# Profile 2: Mid-Tier Indies / Community Favorites (Moderate price, high playtime)
# Profile 3: Achievement-Enabled Modest Indies (Low price, Standard achievements)
# Profile 4: Minimalist / Budget Barebone (Lowest price, 0 achievements, 0 DLC)

v7_candidates$cluster_id <- km_model$cluster
v7_candidates$PC1 <- pca_model$x[, 1]
v7_candidates$PC2 <- pca_model$x[, 2]
v7_candidates$PC3 <- pca_model$x[, 3]

# Compute medians to identify cluster semantics
cluster_stats <- v7_candidates[, .(
  N = .N,
  med_price = as.numeric(median(price)),
  med_reviews = as.numeric(median(num_reviews_total)),
  med_ccu = as.numeric(median(peak_ccu)),
  med_playtime = as.numeric(median(average_playtime_forever)),
  med_achieve = as.numeric(median(achievements)),
  med_dlc = as.numeric(median(dlc_count)),
  med_pos = as.numeric(median(pct_pos_total))
), by = cluster_id]

# Assign canonical labels based on characteristics:
# Highest reviews & CCU -> Established Commercial Releases
comm_id <- cluster_stats[order(-med_reviews)][1]$cluster_id
# Lowest achievements and lowest DLC -> Minimalist / Budget Barebone
min_id  <- cluster_stats[order(med_achieve, med_dlc)][1]$cluster_id
# Remaining two: one has higher playtime -> Engaged Mid-Tier Indies
rem <- cluster_stats[!cluster_id %in% c(comm_id, min_id)]
mid_id <- rem[order(-med_playtime)][1]$cluster_id
# The other -> Achievement-Enabled Modest Indie Releases
ach_id <- rem[!cluster_id %in% mid_id]$cluster_id

label_map <- c()
label_map[comm_id] <- "Established Commercial / High-Engagement"
label_map[mid_id]  <- "Engaged Mid-Tier Indies / Community Favorites"
label_map[ach_id]  <- "Achievement-Enabled Modest Indie Releases"
label_map[min_id]  <- "Minimalist / Budget Barebone Catalog"

v7_candidates[, cluster_label := label_map[cluster_id]]

# Cluster profile summary for parallel coordinate / radar plot
cluster_profiles <- v7_candidates[, .(
  N = .N,
  pct_catalog = round((.N / nrow(v7_candidates)) * 100, 1),
  Price = as.numeric(median(price)),
  Sentiment = as.numeric(median(pct_pos_total)),
  Reviews = as.numeric(median(num_reviews_total)),
  Peak_CCU = as.numeric(median(peak_ccu)),
  Playtime = as.numeric(median(average_playtime_forever)),
  Achievements = as.numeric(median(achievements)),
  DLC_Count = as.numeric(median(dlc_count))
), by = .(cluster_id, cluster_label)][order(cluster_id)]

# Benchmark notable games to highlight
benchmark_titles <- c(
  "Counter-Strike 2", "Dota 2", "Cyberpunk 2077", "ELDEN RING", "Terraria",
  "Stardew Valley", "Hades", "Hollow Knight", "The Witcher 3: Wild Hunt",
  "Baldur's Gate 3", "Grand Theft Auto V", "Rust", "Euro Truck Simulator 2",
  "Among Us", "Slay the Spire", "Celeste", "Vampire Survivors"
)
benchmarks <- v7_candidates[name %in% benchmark_titles]

# Sample 3,000 background points for instant smooth plotting + all benchmarks
set.seed(42)
sampled_v7 <- rbind(
  benchmarks,
  v7_candidates[!name %in% benchmark_titles][sample(.N, 3000)]
)[!duplicated(steam_appid)]

vis7_data <- list(
  pca_ve = pca_ve,
  pca_loadings = pca_loadings,
  cluster_profiles = cluster_profiles,
  sampled_points = sampled_v7,
  full_search_lookup = v7_candidates[, .(steam_appid, name, cluster_label, price, pct_pos_total, num_reviews_total, peak_ccu, PC1, PC2)],
  benchmarks = benchmarks,
  N_usable = nrow(v7_candidates)
)

# ------------------------------------------------------------------------------
# Cross-Source Bridge & HLTB Auxiliary Analysis
# ------------------------------------------------------------------------------
cat("[SURGE BUILD] Building Auxiliary Cross-Source Data...\n")
bridge_high <- bridge_raw[match_status == "HIGH_CONFIDENCE"]
# Link HLTB duration with Steam ratings where available
bridge_joined <- merge(
  bridge_high[, .(steam_appid, igdb_id)],
  steam_v3[, .(steam_appid, name, price, pct_pos_total, num_reviews_total)],
  by = "steam_appid"
)
bridge_joined <- merge(
  bridge_joined,
  igdb_raw[, .(igdb_id, main, completionist, rating)],
  by = "igdb_id"
)
bridge_joined <- bridge_joined[!is.na(main) & main > 0 & !is.na(pct_pos_total)]

aux_data <- list(
  hltb_steam = bridge_joined[sample(.N, min(3000, .N))],
  bridge_summary = list(
    total = nrow(bridge_raw),
    high_conf = nrow(bridge_high),
    ambiguous = sum(bridge_raw$match_status == "AMBIGUOUS_REVIEW")
  )
)

# Save entire bundle to cache
surge_cache <- list(
  meta = meta,
  vis1 = vis1_data,
  vis2 = vis2_data,
  vis3 = vis3_data,
  vis4 = vis4_data,
  vis5 = vis5_data,
  vis6 = vis6_data,
  vis7 = vis7_data,
  aux  = aux_data
)

saveRDS(surge_cache, "data/surge_cache.rds")
cat("[SURGE BUILD] SUCCESS: Cache saved to data/surge_cache.rds (Size: ",
    round(file.size("data/surge_cache.rds") / (1024 * 1024), 2), " MB)\n")
