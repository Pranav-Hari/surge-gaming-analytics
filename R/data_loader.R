# ==============================================================================
# SURGE — Data Loader Module
# Loads verified cached analytical structures and provides data query helpers
# ==============================================================================

library(data.table)

get_surge_data <- function(cache_path = "data/surge_cache.rds") {
  if (!file.exists(cache_path)) {
    message("[SURGE DATA] Cache not found. Generating on the fly...")
    source("build_cache.R", local = TRUE)
  }

  if (!exists(".SURGE_GLOBAL_CACHE", envir = .GlobalEnv)) {
    message("[SURGE DATA] Loading cached data from: ", cache_path)
    assign(".SURGE_GLOBAL_CACHE", readRDS(cache_path), envir = .GlobalEnv)
  }

  return(get(".SURGE_GLOBAL_CACHE", envir = .GlobalEnv))
}
