# ==============================================================================
# SURGE — Module: Visualization 3 (Value Proposition)
# Steam Pricing, Discounts, and Sentiment Dynamics (Bubble Scatter Plot with Loess)
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(data.table)

vis3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Row 1: Primary Scatter Visualization & Pricing Controls
    div(
      class = "row g-3 mb-3",
      # Controls Sidebar
      div(
        class = "col-lg-4 col-xl-3",
        surge_card(
          title = "Pricing Controls",
          subtitle = "Filter price range and commercial terms",
          content = tagList(
            sliderInput(
              ns("price_cap"),
              tags$b("Maximum Price Cap ($ USD):"),
              min = 10, max = 70, value = 70, step = 5
            ),
            checkboxInput(
              ns("include_f2p"),
              tags$b("Include Free-to-Play ($0.00) Games"),
              value = TRUE
            ),
            radioButtons(
              ns("discount_filter"),
              tags$b("Discount Status:"),
              choices = c("All Games" = "all", "Currently Discounted (> 0%)" = "discounted", "Full Price Only" = "full"),
              selected = "all"
            ),
            checkboxInput(
              ns("show_trend"),
              tags$b("Display Smoothed Trend Line (Loess)"),
              value = TRUE
            ),
            hr(style = "border-color: var(--border-subtle);"),
            tags$ul(
              class = "small text-muted ps-3 mb-0",
              tags$li("Includes 38,032 games meeting the 30-review threshold (29,616 paid, 8,416 free-to-play)."),
              tags$li("Bubble size scales with total review count on a logarithmic scale."),
              tags$li("Trend line shows local smoothed regression across prices.")
            )
          )
        )
      ),

      # Main Bubble Scatter Plot
      div(
        class = "col-lg-8 col-xl-9",
        surge_card(
          title = "Steam Pricing vs Community Reception",
          subtitle = "Base price in USD compared with positive review percentage",
          content = plotlyOutput(ns("bubble_plot"), height = "480px"),
          takeaway = "Base game price has almost no relationship with player satisfaction (+0.08). Budget indie games achieve positive review rates comparable to $60–$70 releases.",
          limitation = "Measures correlation, not causation. Base price does not account for microtransactions, discounts, or regional pricing differences."
        )
      )
    ),

    # Row 2: Pricing Tier Analysis & Empirical Acclaim Metrics
    div(
      class = "row g-3",
      # Acclaim Metrics Summary Card
      div(
        class = "col-lg-5 col-xl-4",
        surge_card(
          title = "Commercial Acclaim Metrics",
          subtitle = "Statistical benchmarks across price categories",
          content = tagList(
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Price vs Satisfaction:"),
                tags$b(style = "color: var(--brand-gold);", "Near-Zero Link (+0.08)")
            ),
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Typical Paid Game Price:"),
                tags$b("$4.99 USD")
            ),
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Free Games Median Rating:"),
                tags$b("81.0% Positive")
            ),
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Paid Games Median Rating:"),
                tags$b("83.0% Positive")
            ),
            p(
              class = "small text-muted mt-2 mb-0",
              "Higher-priced games do not earn higher satisfaction than budget titles. Retail price reflects monetization strategy rather than perceived game quality."
            ),
            p(
              class = "small text-muted mt-1 mb-0",
              tags$i("This relationship describes an observed market pattern, not proof of cause and effect.")
            )
          )
        )
      ),

      # Tier breakdown bar chart
      div(
        class = "col-lg-7 col-xl-8",
        surge_card(
          title = "Sentiment Tier Distribution by Price Bracket",
          subtitle = "Median positive sentiment across pricing brackets",
          content = plotlyOutput(ns("tier_price_bar"), height = "260px")
        )
      )
    )
  )
}

vis3_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Filtered dataset
    filtered_bubbles <- reactive({
      df <- copy(data$vis3$display_sample)

      # Price cap
      df <- df[price <= input$price_cap]

      # F2P toggle
      if (!input$include_f2p) {
        df <- df[price > 0]
      }

      # Discount filter
      if (input$discount_filter == "discounted") {
        df <- df[discount > 0]
      } else if (input$discount_filter == "full") {
        df <- df[discount == 0 | is.na(discount)]
      }

      df
    })

    # Bubble plot
    output$bubble_plot <- renderPlotly({
      df <- filtered_bubbles()
      validate(need(nrow(df) > 0, "No titles match the selected pricing filters."))

      # Color mapping for review score desc
      tier_colors <- c(
        "Overwhelmingly Positive" = "#00f0ff",
        "Very Positive"           = "#10b981",
        "Positive"                = "#38bdf8",
        "Mostly Positive"         = "#8b5cf6",
        "Mixed"                   = "#f59e0b",
        "Mostly Negative"         = "#f97316",
        "Negative"                = "#f43f5e",
        "Very Negative"           = "#e11d48",
        "Overwhelmingly Negative" = "#991b1b"
      )

      p <- plot_ly()

      # Group by tier for legend
      tiers <- unique(df$review_score_desc)
      for (tier in tiers) {
        sub <- df[review_score_desc == tier]
        color_val <- if (tier %in% names(tier_colors)) tier_colors[[tier]] else "#94a3b8"

        p <- p %>% add_trace(
          data = sub,
          x = ~price,
          y = ~pct_pos_total,
          type = "scatter",
          mode = "markers",
          name = tier,
          marker = list(
            size = ~pmax(4, log_reviews * 2.8),
            color = color_val,
            opacity = 0.72,
            line = list(color = "#ffffff", width = 0.5)
          ),
          text = ~paste0(
            "<b>", name, "</b><br>",
            "Price: $", sprintf("%.2f", price), " (Discount: ", discount, "%)<br>",
            "Positive Reviews: ", pct_pos_total, "%<br>",
            "Total Reviews: ", format(num_reviews_total, big.mark = ","), "<br>",
            "Review Tier: ", review_score_desc
          ),
          hoverinfo = "text"
        )
      }

      # Optional Loess Smoother
      if (input$show_trend && nrow(df) >= 30) {
        loess_fit <- suppressWarnings(loess(pct_pos_total ~ price, data = df, span = 0.65))
        grid_price <- seq(min(df$price), max(df$price), length.out = 100)
        loess_pred <- predict(loess_fit, newdata = data.frame(price = grid_price))

        p <- p %>% add_trace(
          x = grid_price,
          y = loess_pred,
          type = "scatter",
          mode = "lines",
          name = "Loess Trend Line",
          line = list(color = "#ffffff", width = 3, dash = "solid"),
          hoverinfo = "none"
        )
      }

      p <- p %>% layout(
        xaxis = list(title = "Base Retail Price ($ USD)", range = c(-1, input$price_cap + 1)),
        yaxis = list(title = "Positive Review Percentage (% Positive)", range = c(15, 102)),
        legend = list(orientation = "h", x = 0, y = 1.1)
      )

      surge_plotly_layout(p, height = 480)
    })

    # Tier breakdown bar chart
    output$tier_price_bar <- renderPlotly({
      df <- filtered_bubbles()
      validate(need(nrow(df) > 0, ""))

      # Bin price
      df[, Price_Bracket := fifelse(price == 0, "Free ($0)",
                            fifelse(price <= 10, "$0.01–$10",
                            fifelse(price <= 20, "$10.01–$20",
                            fifelse(price <= 40, "$20.01–$40", "$40.01+"))))]

      bracket_order <- c("Free ($0)", "$0.01–$10", "$10.01–$20", "$20.01–$40", "$40.01+")
      df[, Bracket_ord := factor(Price_Bracket, levels = bracket_order)]

      agg <- df[, .(
        Game_Count = .N,
        Median_Pos = round(median(pct_pos_total), 1),
        Avg_Pos = round(mean(pct_pos_total), 1)
      ), by = Bracket_ord][order(Bracket_ord)]

      p <- plot_ly(
        data = agg,
        x = ~Bracket_ord,
        y = ~Median_Pos,
        type = "bar",
        marker = list(
          color = c("#38bdf8", "#00f0ff", "#10b981", "#a855f7", "#f59e0b"),
          line = list(color = "#ffffff", width = 0.5)
        ),
        hovertemplate = "<b>%{x}</b><br>Median Positive: %{y}%<br>Sample Games: %{customdata:,}<extra></extra>",
        customdata = ~Game_Count
      ) %>%
      layout(
        xaxis = list(title = "Pricing Bracket"),
        yaxis = list(title = "Median Positive Sentiment (%)", range = c(70, 95)),
        margin = list(t = 20, b = 35)
      )

      surge_plotly_layout(p, height = 240)
    })
  })
}
