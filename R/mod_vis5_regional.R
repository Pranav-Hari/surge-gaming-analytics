# ==============================================================================
# SURGE — Module: Visualization 5 (Global Footprint)
# Regional Historical Market Breakdown (100% Stacked & Absolute Sales by Region)
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(data.table)
library(DT)

vis5_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row g-3 mb-3",
      # Filter Controls Sidebar
      div(
        class = "col-lg-3",
        surge_card(
          title = "Regional Filters",
          subtitle = "Segment by grouping, metric, and historical era",
          content = tagList(
            radioButtons(
              ns("group_by"),
              tags$b("Primary Grouping:"),
              choices = c("Genre (12 Categories)" = "genre", "Platform Ecosystem" = "platform"),
              selected = "genre"
            ),
            radioButtons(
              ns("display_mode"),
              tags$b("Display Metric:"),
              choices = c("100% Percentage Share (%)" = "pct", "Absolute Sales Volume (Millions)" = "abs"),
              selected = "pct"
            ),
            selectInput(
              ns("era_filter"),
              tags$b("Historical Console Era:"),
              choices = c(
                "All Eras Combined (1980–2016)" = "all",
                "8/16-Bit Era (1980–1995)" = "retro",
                "32/128-Bit Era (1996–2005)" = "classic",
                "HD Console Era (2006–2016)" = "hd"
              ),
              selected = "all"
            ),
            hr(style = "border-color: var(--border-subtle);"),
            tags$ul(
              class = "small text-muted ps-3 mb-0",
              tags$li("North America: 4,343.2M units (49.2% lifetime share)."),
              tags$li("Europe: 2,400.2M units (27.2%); Japan: 1,290.7M units (14.6%)."),
              tags$li("Covers 16,446 verified physical console releases from 1980 to 2016.")
            )
          )
        ),
        # Regional Legend KPI Card
        surge_card(
          title = "Territory Regional Shares",
          content = tagList(
            div(class = "d-flex justify-content-between mb-1",
                span(tags$span(style = "color: #38bdf8;", "■ "), "North America:"),
                tags$b("49.2% ($4,343M)")
            ),
            div(class = "d-flex justify-content-between mb-1",
                span(tags$span(style = "color: #f59e0b;", "■ "), "Europe:"),
                tags$b("27.2% ($2,400M)")
            ),
            div(class = "d-flex justify-content-between mb-1",
                span(tags$span(style = "color: #f43f5e;", "■ "), "Japan:"),
                tags$b("14.6% ($1,291M)")
            ),
            div(class = "d-flex justify-content-between",
                span(tags$span(style = "color: #64748b;", "■ "), "Other:"),
                tags$b("8.9% ($783M)")
            )
          )
        )
      ),

      # Main Stacked Bar Chart
      div(
        class = "col-lg-9",
        surge_card(
          title = "Regional Historical Market Breakdown",
          subtitle = "Distribution of retail console sales across North America, Europe, Japan, and other regions",
          content = plotlyOutput(ns("regional_bar_plot"), height = "560px"),
          takeaway = "North America accounted for nearly half of global console sales (49.2%), while Japan drove 22.4% of RPG sales but under 1% of Xbox sales. Europe contributed steady demand of 25–30% across all genres.",
          limitation = "Reflects historical retail disc and cartridge sales up to 2016; does not include modern digital console or mobile downloads."
        )
      )
    ),

    # Regional Table Summary
    div(
      class = "row",
      div(
        class = "col-12",
        surge_card(
          title = "Regional Sales Data Breakdown",
          subtitle = "Underlying unit volumes (Millions) and territorial percentage distributions",
          content = tagList(
            DTOutput(ns("regional_table")),
            p(
              class = "small text-muted mt-2 mb-0",
              tags$i("Unknown means the original dataset did not provide a genre for these games. Their sales are included in the totals but cannot be assigned to a known genre.")
            )
          )
        )
      )
    )
  )
}

vis5_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Filter dataset by Era
    filtered_df <- reactive({
      df <- copy(data$vis5$raw)

      if (input$era_filter == "retro") {
        df <- df[Year_int <= 1995]
      } else if (input$era_filter == "classic") {
        df <- df[Year_int >= 1996 & Year_int <= 2005]
      } else if (input$era_filter == "hd") {
        df <- df[Year_int >= 2006 & Year_int <= 2016]
      }

      # Clean missing/blank genre labels to "Unknown" to match dataset's missing category mapping
      df[is.na(Genre) | trimws(Genre) == "", Genre := "Unknown"]

      df
    })

    # Aggregate data by grouping
    aggregated_data <- reactive({
      df <- filtered_df()
      validate(need(nrow(df) > 0, "No records for selected era."))

      grp_col <- if (input$group_by == "genre") "Genre" else "Platform_Family"

      agg <- df[, .(
        NA_Sales = round(sum(NA_Sales), 2),
        EU_Sales = round(sum(EU_Sales), 2),
        JP_Sales = round(sum(JP_Sales), 2),
        Other_Sales = round(sum(Other_Sales), 2),
        Global_Sales = round(sum(Global_Sales), 2),
        Games = .N
      ), by = grp_col][order(Global_Sales)]

      setnames(agg, grp_col, "Category")

      # Compute percentages
      agg[, NA_Pct := round((NA_Sales / Global_Sales) * 100, 1)]
      agg[, EU_Pct := round((EU_Sales / Global_Sales) * 100, 1)]
      agg[, JP_Pct := round((JP_Sales / Global_Sales) * 100, 1)]
      agg[, Other_Pct := round((Other_Sales / Global_Sales) * 100, 1)]

      agg
    })

    # Regional Bar Plot (100% Stacked or Absolute)
    output$regional_bar_plot <- renderPlotly({
      df <- aggregated_data()
      is_pct <- (input$display_mode == "pct")

      p <- plot_ly(data = df)

      # Colors
      c_na <- "#38bdf8"
      c_eu <- "#f59e0b"
      c_jp <- "#f43f5e"
      c_ot <- "#64748b"

      if (is_pct) {
        # 100% Stacked
        p <- p %>%
          add_trace(
            x = ~NA_Pct, y = ~Category, type = "bar", orientation = "h", name = "North America",
            marker = list(color = c_na, line = list(color = "#000", width = 0.5)),
            text = ~paste0(NA_Pct, "%"), textposition = "inside", insidetextfont = list(color = "#fff", size = 10),
            hovertemplate = "<b>%{y}</b><br>North America: %{x}% (%{customdata:.2f}M Units)<extra></extra>",
            customdata = ~NA_Sales
          ) %>%
          add_trace(
            x = ~EU_Pct, y = ~Category, type = "bar", orientation = "h", name = "Europe",
            marker = list(color = c_eu, line = list(color = "#000", width = 0.5)),
            text = ~paste0(EU_Pct, "%"), textposition = "inside", insidetextfont = list(color = "#fff", size = 10),
            hovertemplate = "<b>%{y}</b><br>Europe: %{x}% (%{customdata:.2f}M Units)<extra></extra>",
            customdata = ~EU_Sales
          ) %>%
          add_trace(
            x = ~JP_Pct, y = ~Category, type = "bar", orientation = "h", name = "Japan",
            marker = list(color = c_jp, line = list(color = "#000", width = 0.5)),
            text = ~paste0(JP_Pct, "%"), textposition = "inside", insidetextfont = list(color = "#fff", size = 10),
            hovertemplate = "<b>%{y}</b><br>Japan: %{x}% (%{customdata:.2f}M Units)<extra></extra>",
            customdata = ~JP_Sales
          ) %>%
          add_trace(
            x = ~Other_Pct, y = ~Category, type = "bar", orientation = "h", name = "Other",
            marker = list(color = c_ot, line = list(color = "#000", width = 0.5)),
            text = ~paste0(Other_Pct, "%"), textposition = "inside", insidetextfont = list(color = "#fff", size = 10),
            hovertemplate = "<b>%{y}</b><br>Other: %{x}% (%{customdata:.2f}M Units)<extra></extra>",
            customdata = ~Other_Sales
          ) %>%
          layout(
            barmode = "stack",
            xaxis = list(title = "Percentage Share of Global Sales (%)", range = c(0, 100)),
            yaxis = list(title = "", tickfont = list(size = 11)),
            legend = list(orientation = "h", x = 0, y = 1.08)
          )
      } else {
        # Absolute Sales
        p <- p %>%
          add_trace(
            x = ~NA_Sales, y = ~Category, type = "bar", orientation = "h", name = "North America",
            marker = list(color = c_na),
            hovertemplate = "<b>%{y}</b><br>North America: %{x:.2f}M Units<extra></extra>"
          ) %>%
          add_trace(
            x = ~EU_Sales, y = ~Category, type = "bar", orientation = "h", name = "Europe",
            marker = list(color = c_eu),
            hovertemplate = "<b>%{y}</b><br>Europe: %{x:.2f}M Units<extra></extra>"
          ) %>%
          add_trace(
            x = ~JP_Sales, y = ~Category, type = "bar", orientation = "h", name = "Japan",
            marker = list(color = c_jp),
            hovertemplate = "<b>%{y}</b><br>Japan: %{x:.2f}M Units<extra></extra>"
          ) %>%
          add_trace(
            x = ~Other_Sales, y = ~Category, type = "bar", orientation = "h", name = "Other",
            marker = list(color = c_ot),
            hovertemplate = "<b>%{y}</b><br>Other: %{x:.2f}M Units<extra></extra>"
          ) %>%
          layout(
            barmode = "stack",
            xaxis = list(title = "Unit Sales (Millions)"),
            yaxis = list(title = "", tickfont = list(size = 11)),
            legend = list(orientation = "h", x = 0, y = 1.08)
          )
      }

      surge_plotly_layout(p, height = 540)
    })

    # Regional Table
    output$regional_table <- renderDT({
      df <- aggregated_data()

      # Reverse so highest global sales is on top
      df_rev <- df[order(-Global_Sales)]

      dt_display <- data.frame(
        `Category` = df_rev$Category,
        `Titles` = format(df_rev$Games, big.mark = ","),
        `Global Total (M)` = paste0(format(round(df_rev$Global_Sales, 1), big.mark = ","), " M"),
        `North America` = paste0(round(df_rev$NA_Sales, 1), "M (", df_rev$NA_Pct, "%)"),
        `Europe` = paste0(round(df_rev$EU_Sales, 1), "M (", df_rev$EU_Pct, "%)"),
        `Japan` = paste0(round(df_rev$JP_Sales, 1), "M (", df_rev$JP_Pct, "%)"),
        `Other Territories` = paste0(round(df_rev$Other_Sales, 1), "M (", df_rev$Other_Pct, "%)"),
        check.names = FALSE
      )
      surge_datatable(
        dt_display,
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
