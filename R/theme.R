# ==============================================================================
# SURGE — Theme, Color Tokens, and UI Layout Components
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)

# 1. Color Palette Tokens (Option C: Regal Branding)
SURGE_PALETTE <- list(
  bg_primary      = "#08090D", # Rich Black
  bg_card         = "#0e1017", # Neutral Dark Card Surface
  bg_plot         = "#0a0c13", # Neutral Dark Plotting Surface
  border_subtle   = "rgba(201, 162, 39, 0.14)",
  border_glow     = "rgba(201, 162, 39, 0.40)",

  # Option C Brand Accents
  gold            = "#C9A227", # Royal Antique Gold (Primary Brand Accent)
  velvet          = "#4B236A", # Royal Velvet (Secondary Brand Accent)
  crimson         = "#9B111E", # Imperial Crimson (Restrained Emphasis)
  royal_blue      = "#4169E1", # Royal Blue (Supporting Interface Accent)

  # Preserved Analytical Palette (DO NOT ALTER DATA VISUALIZATION PALETTES)
  cyan            = "#00f0ff",
  violet          = "#a855f7",
  blue            = "#38bdf8",
  emerald         = "#10b981",
  amber           = "#f59e0b",
  rose            = "#f43f5e",

  text_main       = "#f1f5f9",
  text_muted      = "#94a3b8",
  text_dim        = "#64748b",
  grid_color      = "rgba(255, 255, 255, 0.05)"
)

# 2. bslib Theme Definition
surge_bs_theme <- function() {
  bs_theme(
    version = 5,
    bg = SURGE_PALETTE$bg_primary,
    fg = SURGE_PALETTE$text_main,
    primary = SURGE_PALETTE$gold,
    secondary = SURGE_PALETTE$velvet,
    success = SURGE_PALETTE$emerald,
    warning = SURGE_PALETTE$amber,
    danger = SURGE_PALETTE$crimson,
    base_font = font_google("Inter"),
    heading_font = font_google("Outfit"),
    code_font = font_google("JetBrains Mono")
  )
}

# 3. Plotly Dark Layout Styling Helper
surge_plotly_layout <- function(p, title = NULL, subtitle = NULL, height = 460) {
  p %>%
    layout(
      title = if (!is.null(title)) {
        list(
          text = if (!is.null(subtitle)) {
            paste0("<b style='color:#ffffff; font-size:14px;'>", title, "</b><br><span style='color:#94a3b8; font-size:11px;'>", subtitle, "</span>")
          } else {
            paste0("<b style='color:#ffffff; font-size:14px;'>", title, "</b>")
          },
          x = 0.01,
          y = 0.98,
          xanchor = "left",
          yanchor = "top"
        )
      } else NULL,
      paper_bgcolor = SURGE_PALETTE$bg_card,
      plot_bgcolor = SURGE_PALETTE$bg_plot,
      font = list(family = "'Inter', sans-serif", color = SURGE_PALETTE$text_main, size = 11),
      hoverlabel = list(
        bgcolor = "#0e1018",
        bordercolor = SURGE_PALETTE$gold,
        font = list(family = "'Inter', sans-serif", color = "#ffffff", size = 12)
      ),
      xaxis = list(
        gridcolor = SURGE_PALETTE$grid_color,
        zerolinecolor = "rgba(255,255,255,0.08)",
        linecolor = "rgba(255,255,255,0.1)",
        tickfont = list(color = SURGE_PALETTE$text_muted, size = 10)
      ),
      yaxis = list(
        gridcolor = SURGE_PALETTE$grid_color,
        zerolinecolor = "rgba(255,255,255,0.08)",
        linecolor = "rgba(255,255,255,0.1)",
        tickfont = list(color = SURGE_PALETTE$text_muted, size = 10)
      ),
      legend = list(
        font = list(color = SURGE_PALETTE$text_muted, size = 10),
        bgcolor = "rgba(14, 16, 24, 0.75)",
        bordercolor = "rgba(201, 162, 39, 0.15)",
        borderwidth = 1
      ),
      margin = list(t = if (!is.null(title)) 50 else 25, b = 40, l = 50, r = 25),
      height = height
    ) %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d", "sendDataToCloud"))
}

# 4. Custom KPI Stat Box
surge_kpi_card <- function(label, value, subtext = NULL) {
  div(
    class = "kpi-card",
    div(class = "kpi-label", label),
    div(class = "kpi-value", value),
    if (!is.null(subtext)) div(class = "kpi-subtext", subtext)
  )
}

# 5. Standardized Glassmorphism Analytics Card
surge_card <- function(title, subtitle = NULL, content, takeaway = NULL, limitation = NULL, badge = NULL) {
  div(
    class = "surge-card",
    div(
      class = "mb-1",
      div(class = "card-title-surge", title)
    ),
    if (!is.null(subtitle)) div(class = "card-subtitle-surge", subtitle),
    content,
    if (!is.null(takeaway)) {
      div(class = "takeaway-box", tags$b("Key Takeaway: "), takeaway)
    },
    if (!is.null(limitation)) {
      div(class = "limitation-box", tags$b("Analytical Limitation: "), limitation)
    }
  )
}

# 6. Standardized DataTables Helper with Optional Page Size Selector
surge_datatable <- function(df, colnames = NULL, pageLength = 10, enable_page_size = FALSE, options = list(), ...) {
  default_opts <- list(
    pageLength = pageLength,
    autoWidth = TRUE,
    drawCallback = DT::JS("function(settings) {
      var api = this.api();
      var p = api.page.info();
      var wrapper = $(this).closest('.dataTables_wrapper');
      if (p.pages <= 1) {
        wrapper.find('.dataTables_paginate').css('display', 'none');
        wrapper.find('.dataTables_info').css('display', 'none');
      } else {
        wrapper.find('.dataTables_paginate').css('display', '');
        wrapper.find('.dataTables_info').css('display', '');
      }
    }")
  )

  if (isTRUE(enable_page_size)) {
    default_opts$lengthMenu <- c(10, 25, 50, 100)
    default_opts$dom <- "lfrtip"
  } else {
    default_opts$lengthChange <- FALSE
    default_opts$dom <- "frtip"
  }

  merged_opts <- utils::modifyList(default_opts, options)

  # Absorb any DataTables options passed in ... to prevent 'unused argument' errors
  extra_args <- list(...)
  known_opt_keys <- c("order", "dom", "searching", "paging", "info", "ordering", "lengthChange", "lengthMenu", "pageLength")
  for (k in intersect(names(extra_args), known_opt_keys)) {
    merged_opts[[k]] <- extra_args[[k]]
    extra_args[[k]] <- NULL
  }

  dt_args <- list(
    data = df,
    options = merged_opts,
    rownames = FALSE,
    class = "compact stripe hover"
  )
  if (!is.null(colnames)) {
    dt_args$colnames <- colnames
  }

  do.call(DT::datatable, c(dt_args, extra_args))
}

# 7. Persistent footer removed per design review
surge_footer <- function() {
  NULL
}
