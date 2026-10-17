# ==============================================================================
# SURGE — Module: Visualization 1 (Ecosystem Architecture)
# Genre × Platform Catalogue Distribution Heatmap Matrix with Drill-Down
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(data.table)

vis1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row g-3 mb-3",
      # Filter Controls Sidebar
      div(
        class = "col-lg-3",
        surge_card(
          title = "Ecosystem Filters",
          subtitle = "Configure platforms and matrix parameters",
          content = tagList(
            checkboxGroupInput(
              ns("platform_filter"),
              tags$b("Hardware Ecosystems:"),
              choices = c("PC", "PlayStation", "Nintendo", "Xbox", "Other / Retro"),
              selected = c("PC", "PlayStation", "Nintendo", "Xbox", "Other / Retro")
            ),
            sliderInput(
              ns("top_genres"),
              tags$b("Number of Top Genres:"),
              min = 8, max = 23, value = 18, step = 1
            ),
            selectInput(
              ns("color_scale"),
              tags$b("Heatmap Color Scale:"),
              choices = c(
                "Cividis (Navy-Gold)" = "Cividis",
                "Inferno (Dark-Red-Yellow)" = "Inferno",
                "Magma (Dark-Purple-Orange)" = "Magma",
                "YlGnBu (Blue-Green)" = "YlGnBu"
              ),
              selected = "Cividis"
            ),
            hr(style = "border-color: var(--border-subtle);"),
            tags$ul(
              class = "text-muted small ps-3 mb-0",
              tags$li("90,815 distinct games across 5 platform families"),
              tags$li("Counts unique titles per cell; multi-genre titles appear in each applicable category")
            )
          )
        ),
        # Platform Catalog Distribution Marginal Card
        surge_card(
          title = "Platform Total Catalog Share",
          content = plotlyOutput(ns("marginal_plat_bar"), height = "230px")
        )
      ),

      # Main Heatmap Matrix
      div(
        class = "col-lg-9",
        surge_card(
          title = "Genre × Platform Catalogue Distribution",
          subtitle = "Number of distinct games across console and PC platform families",
          content = plotlyOutput(ns("heatmap_plot"), height = "520px"),
          takeaway = "PC heavily concentrates in Strategy and Simulation games, while Nintendo leads in Platformers and Adventure titles. PlayStation and Xbox show balanced catalogues across Action and Shooter genres.",
          limitation = "Represents catalogue title counts, not unit sales, player counts, or developer revenue."
        )
      )
    ),

    # Drill-down table
    div(
      class = "row",
      div(
        class = "col-12",
        surge_card(
          title = "Catalogue Explorer & Drill-Down",
          subtitle = "Browse distinct game records within selected platform families and genres",
          content = tagList(
            div(class = "row g-2 mb-2 align-items-center",
                div(class = "col-md-4",
                    selectInput(ns("drill_genre"), "Filter Table by Genre:", choices = c("All Genres"), selected = "All Genres")
                ),
                div(class = "col-md-4",
                    selectInput(ns("drill_plat"), "Filter Table by Platform:", choices = c("All Platforms"), selected = "All Platforms")
                ),
                div(class = "col-md-4 text-md-end text-muted small",
                    "Showing representative high-engagement titles with HLTB metrics"
                )
            ),
            DTOutput(ns("drilldown_table"))
          )
        )
      )
    )
  )
}

vis1_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Initialize drill-down select choices
    observe({
      updateSelectInput(session, "drill_genre", choices = c("All Genres", data$vis1$genre_order))
      updateSelectInput(session, "drill_plat", choices = c("All Platforms", data$vis1$platform_order))
    })

    # Filtered aggregation
    filtered_agg <- reactive({
      req(input$platform_filter)
      df <- data$vis1$agg[platform_family %in% input$platform_filter]

      # Filter top N genres
      top_g <- head(data$vis1$genre_order, input$top_genres)
      df <- df[genre %in% top_g]
      df
    })

    # Heatmap Plot
    output$heatmap_plot <- renderPlotly({
      df <- filtered_agg()
      validate(need(nrow(df) > 0, "Please select at least one platform family."))

      # Pivot to matrix format for heatmap
      genres_sub <- head(data$vis1$genre_order, input$top_genres)
      # Reverse for standard top-to-bottom display
      genres_rev <- rev(genres_sub)
      plats_sub  <- intersect(data$vis1$platform_order, input$platform_filter)

      # Build matrix
      mat <- matrix(0, nrow = length(genres_rev), ncol = length(plats_sub),
                    dimnames = list(genres_rev, plats_sub))
      hover_text <- matrix("", nrow = length(genres_rev), ncol = length(plats_sub))

      for (i in seq_along(genres_rev)) {
        for (j in seq_along(plats_sub)) {
          g <- genres_rev[i]
          p <- plats_sub[j]
          match_row <- df[genre == g & platform_family == p]
          if (nrow(match_row) > 0) {
            cnt <- match_row$game_count[1]
            pct <- match_row$pct_of_platform[1]
            top_t <- match_row$top_titles[1]
            mat[i, j] <- cnt
            hover_text[i, j] <- paste0(
              "<b>Genre:</b> ", g, "<br>",
              "<b>Platform:</b> ", p, "<br>",
              "<b>Distinct Games:</b> ", format(cnt, big.mark = ","), "<br>",
              "<b>Platform Share:</b> ", pct, "%<br>",
              "<b>Notable Titles:</b> ", top_t
            )
          } else {
            hover_text[i, j] <- paste0("<b>Genre:</b> ", g, "<br><b>Platform:</b> ", p, "<br><b>Distinct Games:</b> 0")
          }
        }
      }

      p <- plot_ly(
        x = plats_sub,
        y = genres_rev,
        z = mat,
        type = "heatmap",
        colorscale = input$color_scale,
        zmin = 0,
        text = hover_text,
        hoverinfo = "text",
        colorbar = list(
          title = list(text = "Titles", font = list(color = "#f1f5f9", size = 10)),
          tickfont = list(color = "#94a3b8", size = 9),
          outlinecolor = "rgba(56, 189, 248, 0.2)"
        )
      ) %>%
      layout(
        xaxis = list(title = "", tickangle = -15, gridcolor = "transparent"),
        yaxis = list(title = "", tickfont = list(size = 10), gridcolor = "transparent"),
        margin = list(l = 150, r = 20, t = 10, b = 45)
      )

      surge_plotly_layout(p, height = 520)
    })

    # Marginal platform bar
    output$marginal_plat_bar <- renderPlotly({
      req(input$platform_filter)
      df <- data$vis1$plat_totals[platform_family %in% input$platform_filter]

      p <- plot_ly(
        data = df,
        x = ~total_games,
        y = ~reorder(platform_family, total_games),
        type = "bar",
        orientation = "h",
        marker = list(color = "#00f0ff", line = list(color = "#38bdf8", width = 1)),
        hovertemplate = "<b>%{y}</b>: %{x:,} Titles<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "Total Games", tickfont = list(size = 9)),
        yaxis = list(title = "", tickfont = list(size = 9))
      )
      surge_plotly_layout(p, height = 230) %>%
        layout(margin = list(l = 95, r = 15, t = 10, b = 35))
    })

    # Drill-down DT Table
    output$drilldown_table <- renderDT({
      dt <- copy(data$vis1$sample_lookup)

      if (!is.null(input$drill_genre) && input$drill_genre != "All Genres") {
        dt <- dt[genre == input$drill_genre]
      }
      if (!is.null(input$drill_plat) && input$drill_plat != "All Platforms") {
        dt <- dt[platform_family == input$drill_plat]
      }

      surge_datatable(
        dt,
        colnames = c("Title", "Genre", "Platform Family", "User Rating (/100)", "HLTB Main Story (Hrs)"),
        pageLength = 10,
        enable_page_size = TRUE
      )
    })
  })
}
