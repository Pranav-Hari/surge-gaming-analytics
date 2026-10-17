# ==============================================================================
# SURGE — Module: Visualization 4 (Industry Waves)
# The Dual Evolution of Physical Sales & Digital Releases (Coordinated Dual-Panel)
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(data.table)

vis4_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row g-3 mb-3",
      # Controls Sidebar
      div(
        class = "col-lg-3",
        surge_card(
          title = "Timeline Controls",
          subtitle = "Configure historical observation window",
          content = tagList(
            sliderInput(
              ns("year_range"),
              tags$b("Temporal Window (Years):"),
              min = 1980, max = 2025, value = c(1980, 2025), step = 1, sep = ""
            ),
            radioButtons(
              ns("preset_zoom"),
              tags$b("Preset Era Views:"),
              choices = c(
                "Full Modern History (1980–2025)" = "full",
                "Retail & Digital Overlap (1997–2016)" = "overlap",
                "Digital Explosion Era (2014–2025)" = "digital",
                "Golden Console Era (1995–2010)" = "console"
              ),
              selected = "full"
            ),
            hr(style = "border-color: var(--border-subtle);"),
            tags$ul(
              class = "small text-muted ps-3 mb-0",
              tags$li("Steam measures annual digital title additions (1997–2025)."),
              tags$li("Console data measures physical retail copies sold worldwide (1980–2016)."),
              tags$li("The two metrics track distinct phenomena and are coordinated without merging.")
            )
          )
        ),
        # KPI Stats Card
        surge_card(
          title = "Era Benchmarks",
          content = tagList(
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Physical Peak Year:"),
                tags$b(style = "color: var(--neon-amber);", "2008 (671.8M Units)")
            ),
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Steam Annual Peak:"),
                tags$b(style = "color: var(--brand-gold);", "2024 (17,492 Games)")
            ),
            div(class = "d-flex justify-content-between",
                span(class = "text-muted small", "Excluded Incomplete Dates:"),
                tags$b("273 Records / 99.3M Units")
            )
          )
        )
      ),

      # Main Dual-Panel View
      div(
        class = "col-lg-9",
        # Panel A: Steam Releases
        surge_card(
          title = "Modern Digital Era — Annual Steam PC Releases (1997–2025)",
          subtitle = "Annual count of game releases published on Steam",
          content = plotlyOutput(ns("steam_panel"), height = "280px")
        ),
        # Panel B: Historical Console Sales
        surge_card(
          title = "Physical Retail Era — Global Console Sales & Title Volume (1980–2016)",
          subtitle = "Annual global retail unit sales in millions and distinct console titles",
          content = plotlyOutput(ns("sales_panel"), height = "300px"),
          takeaway = "Physical console retail sales peaked in 2008 at 671.8 million units before contracting. In contrast, PC digital releases grew exponentially after 2014, exceeding 17,000 games released annually.",
          limitation = "These two timelines measure completely different things: physical copies sold versus digital titles published. They must not be combined into a single trend."
        )
      )
    )
  )
}

vis4_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Handle Preset Zoom
    observeEvent(input$preset_zoom, {
      if (input$preset_zoom == "full") {
        updateSliderInput(session, "year_range", value = c(1980, 2025))
      } else if (input$preset_zoom == "overlap") {
        updateSliderInput(session, "year_range", value = c(1997, 2016))
      } else if (input$preset_zoom == "digital") {
        updateSliderInput(session, "year_range", value = c(2014, 2025))
      } else if (input$preset_zoom == "console") {
        updateSliderInput(session, "year_range", value = c(1995, 2010))
      }
    })

    # Filtered Steam Releases (Panel A: 1997-2025)
    steam_filtered <- reactive({
      yr <- input$year_range
      df <- data$vis4$steam_timeline[year >= pmax(1997, yr[1]) & year <= pmin(2025, yr[2])]
      df
    })

    # Filtered Console Sales (Panel B: 1980-2016)
    sales_filtered <- reactive({
      yr <- input$year_range
      df <- data$vis4$sales_timeline[year >= pmax(1980, yr[1]) & year <= pmin(2016, yr[2])]
      df
    })

    # Panel A: Steam Area Plot
    output$steam_panel <- renderPlotly({
      df <- steam_filtered()
      validate(need(nrow(df) > 0, "No Steam data within selected temporal window."))

      p <- plot_ly(
        data = df,
        x = ~year,
        y = ~annual_releases,
        type = "scatter",
        mode = "lines+markers",
        fill = "tozeroy",
        fillcolor = "rgba(0, 240, 255, 0.15)",
        line = list(color = "#00f0ff", width = 2.5),
        marker = list(size = 4, color = "#38bdf8"),
        text = ~paste0("<b>Year: ", year, "</b><br>Steam Releases: ", format(annual_releases, big.mark = ",")),
        hoverinfo = "text"
      ) %>%
      layout(
        xaxis = list(
          title = "Release Year (Digital Era: 1997–2025)",
          range = c(pmax(1997, input$year_range[1]) - 0.5, pmin(2025, input$year_range[2]) + 0.5),
          gridcolor = "rgba(255,255,255,0.06)",
          dtick = 3
        ),
        yaxis = list(
          title = "Annual Steam Releases",
          gridcolor = "rgba(255,255,255,0.06)"
        ),
        annotations = if (input$year_range[1] <= 2014 && input$year_range[2] >= 2014) {
          list(
            list(
              x = 2014,
              y = max(df$annual_releases) * 0.85,
              text = "Steam Direct / Greenlight Surge (2014+)",
              showarrow = TRUE,
              arrowhead = 2,
              arrowcolor = "#00f0ff",
              arrowsize = 0.8,
              font = list(color = "#00f0ff", size = 10)
            )
          )
        } else NULL,
        margin = list(t = 20, b = 25, l = 60, r = 20)
      )

      surge_plotly_layout(p, height = 260)
    })

    # Panel B: Console Retail Sales (Dual Y-Axis)
    output$sales_panel <- renderPlotly({
      df <- sales_filtered()
      validate(need(nrow(df) > 0, "No Historical Console Sales within selected temporal window."))

      p <- plot_ly(data = df) %>%
        # Bar: Retail Sales (Millions)
        add_trace(
          x = ~year,
          y = ~retail_sales_millions,
          type = "bar",
          name = "Retail Sales (M Units)",
          marker = list(
            color = "rgba(245, 158, 11, 0.75)",
            line = list(color = "#f59e0b", width = 1)
          ),
          text = ~paste0(
            "<b>Year: ", year, "</b><br>",
            "Global Sales: ", retail_sales_millions, "M Units<br>",
            "Distinct Titles: ", release_count, "<br>",
            "Top Title: ", top_game
          ),
          hoverinfo = "text",
          yaxis = "y"
        ) %>%
        # Line: Release Count (Right Axis)
        add_trace(
          x = ~year,
          y = ~release_count,
          type = "scatter",
          mode = "lines+markers",
          name = "Console Releases",
          line = list(color = "#a855f7", width = 2),
          marker = list(size = 5, color = "#ec4899"),
          yaxis = "y2",
          hoverinfo = "none"
        ) %>%
        layout(
          xaxis = list(
            title = "Release Year (Physical Retail Era: 1980–2016)",
            range = c(pmax(1980, input$year_range[1]) - 0.5, pmin(2016, input$year_range[2]) + 0.5),
            gridcolor = "rgba(255,255,255,0.06)",
            dtick = 4
          ),
          yaxis = list(
            title = "Retail Sales (Millions of Units)",
            titlefont = list(color = "#f59e0b"),
            tickfont = list(color = "#f59e0b"),
            gridcolor = "rgba(255,255,255,0.06)"
          ),
          yaxis2 = list(
            title = "Annual Console Releases",
            titlefont = list(color = "#a855f7"),
            tickfont = list(color = "#a855f7"),
            overlaying = "y",
            side = "right",
            gridcolor = "transparent"
          ),
          legend = list(orientation = "h", x = 0.05, y = 1.15),
          annotations = if (input$year_range[1] <= 2008 && input$year_range[2] >= 2008) {
            list(
              list(
                x = 2008,
                y = 671.8,
                text = "Retail Peak: 2008 (671.8M Units)",
                showarrow = TRUE,
                arrowhead = 2,
                arrowcolor = "#f59e0b",
                font = list(color = "#f59e0b", size = 10)
              )
            )
          } else NULL,
          margin = list(t = 30, b = 40, l = 60, r = 60)
        )

      surge_plotly_layout(p, height = 280)
    })
  })
}
