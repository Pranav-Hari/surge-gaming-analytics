# ==============================================================================
# SURGE — Module: Visualization 2 (Voice of the Player)
# Genre-Wise Community Reception Profiling (Violin & Boxplots with Threshold Filter)
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(data.table)
library(DT)

vis2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row g-3 mb-3",
      # Filter Controls Sidebar
      div(
        class = "col-lg-3",
        surge_card(
          title = "Sentiment Controls",
          subtitle = "Dynamic review volume & monetization filters",
          content = tagList(
            sliderInput(
              ns("min_reviews"),
              tags$b("Minimum Review Threshold (N):"),
              min = 30, max = 500, value = 30, step = 10
            ),
            radioButtons(
              ns("monetization_filter"),
              tags$b("Monetization Model:"),
              choices = c("All Games" = "all", "Paid Only ($ > 0)" = "paid", "Free-to-Play Only ($0)" = "free"),
              selected = "all"
            ),
            div(
              class = "sentiment-genre-grid",
              checkboxGroupInput(
                ns("selected_genres"),
                tags$b("Include Genres:"),
                choices = c("Action", "Adventure", "RPG", "Strategy", "Simulation", "Casual", "Indie", "Massively Multiplayer", "Racing", "Sports"),
                selected = c("Action", "Adventure", "RPG", "Strategy", "Simulation", "Casual", "Indie", "Massively Multiplayer", "Racing", "Sports")
              )
            ),
            hr(style = "border-color: var(--border-subtle);"),
            tags$ul(
              class = "text-muted small ps-3 mb-0",
              tags$li("Minimum 30 reviews required to remove 1-review shovelware distortion"),
              tags$li("Includes 38,032 eligible Steam PC titles (1997–2025)"),
              tags$li("Sentiment represents percentage of positive recommendations (0–100%)")
            )
          )
        ),
        # KPI Summary Card
        surge_card(
          title = "Sample & Catalog Health",
          content = tagList(
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Active Sample Size:"),
                tags$b(textOutput(ns("active_sample_txt"), inline = TRUE))
            ),
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Catalog Benchmark Median:"),
                tags$b(style = "color: var(--brand-gold);", "82.0%")
            ),
            div(class = "d-flex justify-content-between",
                span(class = "text-muted small", "Filtered Sample Median:"),
                tags$b(style = "color: var(--neon-emerald);", textOutput(ns("filtered_median_txt"), inline = TRUE))
            )
          )
        )
      ),

      # Main Violin / Boxplot Chart
      div(
        class = "col-lg-9",
        surge_card(
          title = "Genre-Wise Community Reception",
          subtitle = "Percentage of positive Steam reviews across major genres (minimum 30 reviews)",
          content = plotlyOutput(ns("violin_plot"), height = "490px"),
          takeaway = "Strategy, RPG, and Indie games show consistently high community satisfaction (median 80–82%). Massively Multiplayer titles show wide polarization with significantly lower median ratings (68%).",
          limitation = "Community sentiment reflects the percentage of positive recommendations, not critic review scores. Multi-genre titles are included in each matching genre."
        )
      )
    ),

    # Genre Summary Statistics Table
    div(
      class = "row",
      div(
        class = "col-12",
        surge_card(
          title = "Genre Sentiment Metrics",
          subtitle = "Summary statistics for each genre under the selected review threshold",
          content = DTOutput(ns("genre_stat_table"))
        )
      )
    )
  )
}

vis2_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Reactive filtered dataset
    filtered_data <- reactive({
      req(input$selected_genres)
      df <- data$vis2$raw_unnested[num_reviews_total >= input$min_reviews]

      if (input$monetization_filter == "paid") {
        df <- df[price > 0]
      } else if (input$monetization_filter == "free") {
        df <- df[is_free == TRUE | price == 0]
      }

      df <- df[genre %in% input$selected_genres]
      df
    })

    # KPIs
    output$active_sample_txt <- renderText({
      df <- filtered_data()
      paste0(format(uniqueN(df$steam_appid), big.mark = ","), " games")
    })

    output$filtered_median_txt <- renderText({
      df <- filtered_data()
      if (nrow(df) == 0) return("N/A")
      paste0(round(median(df$pct_pos_total), 1), "%")
    })

    # Plotly Violin + Boxplot
    output$violin_plot <- renderPlotly({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No games meet the selected filters."))

      # Order genres by median
      genre_medians <- df[, .(med = median(pct_pos_total)), by = genre][order(-med)]
      df[, genre_ord := factor(genre, levels = genre_medians$genre)]

      p <- plot_ly(
        data = df,
        x = ~genre_ord,
        y = ~pct_pos_total,
        type = "violin",
        split = ~genre_ord,
        box = list(visible = TRUE, width = 0.25, line = list(color = "#ffffff")),
        meanline = list(visible = TRUE),
        points = FALSE,
        colors = c("#00f0ff", "#a855f7", "#38bdf8", "#10b981", "#f59e0b", "#f43f5e", "#ec4899", "#8b5cf6", "#06b6d4", "#64748b"),
        showlegend = FALSE,
        hoverinfo = "y"
      ) %>%
      layout(
        xaxis = list(title = "", tickangle = -20, tickfont = list(size = 11)),
        yaxis = list(title = "Positive Reviews (% Positive)", range = c(0, 102)),
        shapes = list(
          # Horizontal catalog benchmark median line (82%)
          list(
            type = "line",
            x0 = -0.5,
            x1 = length(unique(df$genre)) - 0.5,
            y0 = 82.0,
            y1 = 82.0,
            line = list(color = "#00f0ff", width = 2, dash = "dash")
          )
        ),
        annotations = list(
          list(
            x = length(unique(df$genre)) - 0.5,
            y = 84.0,
            text = "Catalog Benchmark Median (82%)",
            showarrow = FALSE,
            font = list(color = "#00f0ff", size = 10),
            xanchor = "right"
          )
        )
      )

      surge_plotly_layout(p, height = 490)
    })

    # Genre Summary Statistics Table
    output$genre_stat_table <- renderDT({
      df <- filtered_data()
      if (nrow(df) == 0) return(NULL)

      summary_dt <- df[, .(
        `Game Count` = format(.N, big.mark = ","),
        `Median Pos (%)` = paste0(round(median(pct_pos_total), 1), "%"),
        `25th Percentile` = paste0(round(quantile(pct_pos_total, 0.25), 1), "%"),
        `75th Percentile` = paste0(round(quantile(pct_pos_total, 0.75), 1), "%"),
        `IQR` = paste0(round(IQR(pct_pos_total), 1), "%"),
        `Mean Pos (%)` = paste0(round(mean(pct_pos_total), 1), "%")
      ), by = .(Genre = genre)][order(-as.numeric(gsub("%", "", `Median Pos (%)`)))]

      surge_datatable(
        summary_dt,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          dom = "t"
        )
      )
    })
  })
}
