suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

cat("Checking Steam clean...\n")
steam <- fread("Datasets/Processed/steam_games_clean.csv")
cat("Steam rows:", nrow(steam), "cols:", ncol(steam), "\n")

v2_steam <- steam[!is.na(pct_pos_total) & num_reviews_total >= 30]
cat("Vis 2/3/7 candidate rows (reviews >= 30):", nrow(v2_steam), "\n")
cat("Paid count:", sum(v2_steam$price > 0), "Free count:", sum(v2_steam$price == 0), "\n")

cat("\nChecking Historical sales...\n")
hist <- fread("Datasets/Processed/historical_console_sales.csv")
cat("Hist rows:", nrow(hist), "\n")
hist_clean <- hist[flag_suspicious_year == FALSE | flag_suspicious_year == "False"]
cat("Hist clean rows:", nrow(hist_clean), "Global sales sum:", round(sum(hist_clean$Global_Sales), 2), "\n")

cat("\nChecking IGDB & platforms...\n")
igdb <- fread("Datasets/Processed/multiplatform_igdb_hltb.csv")
plat <- fread("Datasets/Processed/game_platforms.csv")
cat("IGDB rows:", nrow(igdb), "plat rows:", nrow(plat), "\n")

cat("\nDone!\n")
