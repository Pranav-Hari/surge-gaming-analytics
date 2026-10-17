# ==============================================================================
# SURGE — Module: Cross-Source Bridge & Campaign Duration Explorer (Auxiliary)
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(data.table)

cross_drilldown_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row g-3 mb-3",
      # Sidebar
      div(
        class = "col-lg-3",
        surge_card(
          title = "Bridge Explorer Controls",
          subtitle = "Cross-source concordance & playtime filters",
          content = tagList(
            sliderInput(
              ns("max_hours"),
              tags$b("Maximum Story Length (Hours):"),
              min = 10, max = 150, value = 80, step = 5
            ),
            selectInput(
              ns("price_tier"),
              tags$b("Price Tier:"),
              choices = c("All Tiers" = "all", "Budget ($0–$15)" = "budget", "Mid-Tier ($15–$40)" = "mid", "Premium ($40+)" = "premium"),
              selected = "all"
            ),
            hr(style = "border-color: var(--border-subtle);"),
            tags$ul(
              class = "small text-muted ps-3 mb-0",
              tags$li("42,714 high-confidence links between Steam and IGDB/HLTB records."),
              tags$li("1,851 ambiguous matches are excluded from this analysis."),
              tags$li("Strict 1-to-1 matching ensures no inflated game counts.")
            )
          )
        ),
        # KPI Stats Card
        surge_card(
          title = "Concordance Summary",
          content = tagList(
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "High-Confidence Links:"),
                tags$b(style = "color: var(--brand-gold);", "42,714 (95.8%)")
            ),
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Ambiguous / Review:"),
                tags$b(style = "color: var(--neon-amber);", "1,851 (4.2%)")
            ),
            div(class = "d-flex justify-content-between",
                span(class = "text-muted small", "Multiplication Factor:"),
                tags$b(style = "color: var(--neon-emerald);", "1.000x (Safe)")
            )
          )
        )
      ),

      # Main HLTB Story Length vs Steam Acclaim Plot
      div(
        class = "col-lg-9",
        surge_card(
          title = "Campaign Length (HowLongToBeat) vs Community Acclaim (Steam)",
          subtitle = "Main story completion time in hours compared with Steam positive reviews",
          content = plotlyOutput(ns("hltb_scatter_plot"), height = "440px"),
          takeaway = "Games with focused 10–30 hour story campaigns maintain tight community acclaim (median ~87%), while 80+ hour titles show much greater rating variance.",
          limitation = "Story completion hours are crowd-sourced estimates of main campaign length, distinct from total Steam playtime."
        )
      )
    ),

    # Matched Cross-Source Title Inspector Table
    div(
      class = "row",
      div(
        class = "col-12",
        surge_card(
          title = "Verified Cross-Platform Matched Titles",
          subtitle = "Verified games linked across Steam and IGDB/HowLongToBeat datasets",
          content = DTOutput(ns("matched_table"))
        )
      )
    )
  )
}

cross_drilldown_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Filter dataset
    filtered_hltb <- reactive({
      df <- copy(data$aux$hltb_steam)
      df <- df[main <= input$max_hours]

      if (input$price_tier == "budget") {
        df <- df[price <= 15]
      } else if (input$price_tier == "mid") {
        df <- df[price > 15 & price <= 40]
      } else if (input$price_tier == "premium") {
        df <- df[price > 40]
      }

      df
    })

    # Scatter Plot
    output$hltb_scatter_plot <- renderPlotly({
      df <- filtered_hltb()
      validate(need(nrow(df) > 0, "No titles match the selected completion filters."))

      p <- plot_ly(
        data = df,
        x = ~main,
        y = ~pct_pos_total,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = ~pmax(5, log10(num_reviews_total + 1) * 2.5),
          color = ~price,
          colorscale = "Viridis",
          opacity = 0.7,
          colorbar = list(title = "Price ($)", tickfont = list(color = "#94a3b8", size = 9)),
          line = list(color = "#ffffff", width = 0.4)
        ),
        text = ~paste0(
          "<b>", name, "</b><br>",
          "Main Story Length: ", round(main, 1), " Hours<br>",
          "Completionist: ", round(completionist, 1), " Hours<br>",
          "Steam Positive: ", pct_pos_total, "%<br>",
          "Base Price: $", sprintf("%.2f", price), "<br>",
          "Steam Reviews: ", format(num_reviews_total, big.mark = ",")
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        xaxis = list(title = "HLTB Main Story Completion Length (Hours)", range = c(0, input$max_hours + 2)),
        yaxis = list(title = "Steam Community Sentiment (% Positive)", range = c(30, 102))
      )

      surge_plotly_layout(p, height = 420)
    })

    # DT Matched Table
    output$matched_table <- renderDT({
      df <- filtered_hltb()
      dt_display <- df[, .(
        `Title` = name,
        `Main Story (Hrs)` = round(main, 1),
        `Completionist (Hrs)` = round(completionist, 1),
        `Steam % Positive` = paste0(pct_pos_total, "%"),
        `Steam Reviews` = format(num_reviews_total, big.mark = ","),
        `Retail Price` = paste0("$", sprintf("%.2f", price))
      )]

      surge_datatable(
        dt_display,
        pageLength = 10,
        enable_page_size = TRUE
      )
    })
  })
}
