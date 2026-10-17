suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
})

cat("--- Testing Vis 1 (Genre x Platform) ---\n")
igdb <- fread("Datasets/Processed/multiplatform_igdb_hltb.csv", select = c("igdb_id", "genre_names"))
plat <- fread("Datasets/Processed/game_platforms.csv", select = c("igdb_id", "platform_family"))

# Filter non-empty
igdb_valid <- igdb[genre_names != "" & !is.na(genre_names)]
plat_valid <- plat[platform_family != "" & !is.na(platform_family)]
joined_vis1 <- merge(igdb_valid, plat_valid, by = "igdb_id")

cat("Distinct games in joined:", uniqueN(joined_vis1$igdb_id), "\n")

# Unnest genres
vis1_pairs <- joined_vis1[, .(genre = trimws(unlist(strsplit(genre_names, ",")))), by = .(igdb_id, platform_family)]
vis1_pairs <- vis1_pairs[genre != ""]
cat("Total unnested pairs:", nrow(vis1_pairs), "\n")
cat("Distinct games in pairs:", uniqueN(vis1_pairs$igdb_id), "\n")

cat("\n--- Testing Vis 6 (Spearman Correlation) ---\n")
steam <- fread("Datasets/Processed/steam_games_clean.csv")
v6_cols <- c("price", "pct_pos_total", "num_reviews_total", "peak_ccu",
             "average_playtime_forever", "discount", "achievements", "dlc_count")
steam_v6 <- na.omit(steam[, ..v6_cols])
cat("Complete cases for Vis 6:", nrow(steam_v6), "\n")

cat("\n--- Testing Vis 7 (PCA & K-Means) ---\n")
v7_data <- steam[!is.na(pct_pos_total) & num_reviews_total >= 30, .(
  steam_appid, name, price, pct_pos_total, num_reviews_total, peak_ccu,
  average_playtime_forever, achievements, dlc_count
)]
cat("Vis 7 observations:", nrow(v7_data), "\n")

# Transformations
v7_mat <- data.frame(
  price = v7_data$price,
  pct_pos_total = v7_data$pct_pos_total,
  log_reviews = log10(v7_data$num_reviews_total + 1.0),
  log_ccu = log10(v7_data$peak_ccu + 1.0),
  log_playtime = log10(v7_data$average_playtime_forever + 1.0),
  log_achievements = log10(v7_data$achievements + 1.0),
  log_dlc = log10(v7_data$dlc_count + 1.0)
)
v7_scaled <- scale(v7_mat)

# PCA
pca_res <- prcomp(v7_scaled, center = FALSE, scale. = FALSE)
pca_var <- pca_res$sdev^2
pca_ve <- pca_var / sum(pca_var)
cat("PCA Explained Variance:\n")
for (i in 1:4) {
  cat(sprintf("PC%d: %.2f%%\n", i, pca_ve[i] * 100))
}
cat(sprintf("Cumulative PC1-PC3: %.2f%%\n", sum(pca_ve[1:3]) * 100))
cat(sprintf("Cumulative PC1-PC4: %.2f%%\n", sum(pca_ve[1:4]) * 100))

# K-Means
set.seed(42)
km_res <- kmeans(v7_scaled, centers = 4, nstart = 10, iter.max = 50)
cat("K-Means cluster table:\n")
print(table(km_res$cluster))
