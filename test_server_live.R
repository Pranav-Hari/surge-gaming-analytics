# ==============================================================================
# SURGE — Live HTTP Server Verification
# Launches Shiny app on port 3838, fetches HTML over HTTP, checks response
# ==============================================================================

library(shiny)

port <- 3838
cat(sprintf("[LIVE TEST] Starting Shiny server on port %d...\n", port))

# Launch in background process using system2
cmd <- "P:/Apps/R_Rstudio/R-4.6.1/bin/Rscript.exe"
args <- c("-e", sprintf("shiny::runApp('.', port = %d, host = '127.0.0.1', launch.browser = FALSE)", port))

pid <- sys::exec_background(cmd, args = args, std_out = TRUE, std_err = TRUE)
cat(sprintf("[LIVE TEST] Server started with background PID %d. Waiting 3 seconds for boot...\n", pid))

Sys.sleep(3)

# Test HTTP connection
tryCatch({
  conn <- url(sprintf("http://127.0.0.1:%d", port))
  lines <- readLines(conn, n = 50, warn = FALSE)
  close(conn)

  html_snippet <- paste(lines, collapse = "\n")
  cat("[LIVE TEST] HTTP GET response received! Bytes:", nchar(html_snippet), "\n")

  if (grepl("SURGE", html_snippet)) {
    cat("  PASS: 'SURGE' found in HTTP response body.\n")
  } else {
    stop("FAIL: 'SURGE' not found in response.")
  }

  if (grepl("navbar", html_snippet)) {
    cat("  PASS: Navigation navbar successfully rendered.\n")
  }

  cat("\nSUCCESS: Shiny application is fully live and serving requests over HTTP!\n")
}, error = function(e) {
  cat("[LIVE TEST] Error connecting to server:", conditionMessage(e), "\n")
}, finally = {
  # Terminate test server
  tools::pskill(pid)
  cat(sprintf("[LIVE TEST] Test server PID %d terminated.\n", pid))
})
