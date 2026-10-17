# ==============================================================================
# SURGE — Module: Overview & Executive Briefing
# Refined Visual Hierarchy and Readability for 100% Zoom
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)

overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Header Banner
    div(
      class = "surge-card mb-3 p-3",
      style = "background: linear-gradient(135deg, rgba(14, 16, 24, 0.98), rgba(8, 9, 13, 0.95)); border-left: 4px solid var(--brand-gold);",
      div(
        class = "row align-items-center",
        div(
          class = "col-12",
          div(
            class = "d-flex align-items-center gap-3 mb-1 flex-wrap",
            span(class = "hero-wordmark", "SURGE"),
            span(class = "d-none d-sm-inline", style = "color: rgba(255,255,255,0.2); font-size: 1.3rem;", "|"),
            h1(class = "brand-title m-0", style = "font-size: 1.55rem; color: #ffffff; font-weight: 800; display: inline-block;", "Gaming Industry Exposed")
          ),
          p(
            class = "text-muted mb-0 mt-1",
            style = "font-size: 0.88rem; line-height: 1.48; max-width: 1100px;",
            "An empirical visual analytics suite investigating modern PC digital storefronts alongside four decades of physical console retail sales. Navigate through the top bar to inspect catalogue distributions, player sentiment, pricing dynamics, timelines, regional markets, Spearman correlations, and PCA game archetypes."
          )
        )
      )
    ),

    # Key Metrics Row (Balanced 5-column layout)
    div(
      class = "row g-2 mb-3",
      div(class = "col-sm-6 col-lg", uiOutput(ns("kpi_steam"))),
      div(class = "col-sm-6 col-lg", uiOutput(ns("kpi_igdb"))),
      div(class = "col-sm-6 col-lg", uiOutput(ns("kpi_platforms"))),
      div(class = "col-sm-6 col-lg", uiOutput(ns("kpi_sales"))),
      div(class = "col-sm-12 col-lg", uiOutput(ns("kpi_bridge")))
    ),

    # Core Findings & Safeguards Row
    div(
      class = "row g-3 mb-3",
      div(
        class = "col-lg-7",
        surge_card(
          title = "Analytical Modules Overview",
          subtitle = "What each section helps you discover",
          content = div(
            class = "row g-2",
            div(class = "col-12",
                div(class = "p-2 rounded", style = "background: rgba(255,255,255,0.02); border-left: 3px solid var(--brand-gold);",
                    tags$b(style = "color: var(--brand-gold);", "Ecosystem Heatmap: "),
                    "Compares how gaming platforms distribute their game libraries across genres, highlighting platform specialties like Strategy on PC versus Adventure on Nintendo.")
            ),
            div(class = "col-12",
                div(class = "p-2 rounded", style = "background: rgba(255,255,255,0.02); border-left: 3px solid var(--brand-gold);",
                    tags$b(style = "color: var(--brand-gold);", "Player Sentiment: "),
                    "Evaluates user satisfaction across Steam genres to reveal which game categories receive consistent praise and which produce divided opinions.")
            ),
            div(class = "col-12",
                div(class = "p-2 rounded", style = "background: rgba(255,255,255,0.02); border-left: 3px solid var(--brand-gold);",
                    tags$b(style = "color: var(--brand-gold);", "Price vs Reception: "),
                    "Tests whether higher prices lead to better player reviews, comparing free-to-play titles directly against budget indies and premium releases.")
            ),
            div(class = "col-12",
                div(class = "p-2 rounded", style = "background: rgba(255,255,255,0.02); border-left: 3px solid var(--brand-gold);",
                    tags$b(style = "color: var(--brand-gold);", "Dual Timelines: "),
                    "Traces historical physical console sales from 1980 to 2016 alongside the exponential rise of modern digital PC releases from 1997 to 2025.")
            ),
            div(class = "col-12",
                div(class = "p-2 rounded", style = "background: rgba(255,255,255,0.02); border-left: 3px solid var(--brand-gold);",
                    tags$b(style = "color: var(--brand-gold);", "Regional Sales: "),
                    "Explores geographic purchasing preferences across North America, Europe, Japan, and other territories to identify regional genre favorites.")
            ),
            div(class = "col-12",
                div(class = "p-2 rounded", style = "background: rgba(255,255,255,0.02); border-left: 3px solid var(--brand-gold);",
                    tags$b(style = "color: var(--brand-gold);", "Correlations: "),
                    "Examines statistical rank relationships between game prices, player review counts, active player concurrency, and playtime.")
            ),
            div(class = "col-12",
                div(class = "p-2 rounded", style = "background: rgba(255,255,255,0.02); border-left: 3px solid var(--brand-gold);",
                    tags$b(style = "color: var(--brand-gold);", "Game Archetypes: "),
                    "Groups PC games into four natural market segments based on commercial scale, community engagement, and player praise.")
            ),
            div(class = "col-12",
                div(class = "p-2 rounded", style = "background: rgba(255,255,255,0.02); border-left: 3px solid var(--brand-gold);",
                    tags$b(style = "color: var(--brand-gold);", "Cross-Source Bridge: "),
                    "Links titles across Steam, IGDB, and HowLongToBeat to investigate whether campaign completion hours affect player review scores.")
            )
          )
        )
      ),
      div(
        class = "col-lg-5",
        surge_card(
          title = "Analytical Safeguards & Integrity",
          subtitle = "Why these guardrails protect your conclusions",
          content = tags$ul(
            class = "list-unstyled d-flex flex-column gap-2 mb-0",
            tags$li(
              tags$span(style = "color: var(--brand-gold);", "• "),
              tags$b("Catalogue Counts vs Market Share: "),
              "Game counts represent the number of titles catalogued in each dataset, not sales revenue or overall market share."
            ),
            tags$li(
              tags$span(style = "color: var(--brand-gold);", "• "),
              tags$b("Different Eras & Sources: "),
              "Historical console sales (1980–2016 physical retail) and modern Steam data (digital PC releases) cover different timeframes and measure distinct markets."
            ),
            tags$li(
              tags$span(style = "color: var(--brand-gold);", "• "),
              tags$b("Separate Analytical Measures: "),
              "Review percentages, review counts, player concurrency, estimated ownership, sales volume, and completion times measure different things and are never combined into a single score."
            ),
            tags$li(
              tags$span(style = "color: var(--brand-gold);", "• "),
              tags$b("High-Confidence Linking: "),
              "Cross-platform comparisons require verified, high-confidence matches across databases to ensure we never compare unrelated titles."
            ),
            tags$li(
              tags$span(style = "color: var(--brand-gold);", "• "),
              tags$b("Missing Activity Values: "),
              "A missing or zero player activity figure reflects public profile privacy settings or unrecorded tracking, not proof that a game has no players."
            ),
            tags$li(
              tags$span(style = "color: var(--brand-gold);", "• "),
              tags$b("Patterns, Not Proof: "),
              "Correlations and cluster groupings reveal observed patterns and associations across titles, not proof of cause and effect."
            )
          )
        )
      )
    ),

    # Macro Snapshot Charts
    div(
      class = "row g-3",
      div(
        class = "col-lg-6",
        surge_card(
          title = "Ecosystem Hardware Distribution",
          subtitle = "Total distinct titles recorded per platform family in IGDB catalog",
          content = plotlyOutput(ns("overview_platform_bar"), height = "260px")
        )
      ),
      div(
        class = "col-lg-6",
        surge_card(
          title = "Top Historical Console Sales by Genre",
          subtitle = "Lifetime physical unit sales (Millions, 1980–2016)",
          content = plotlyOutput(ns("overview_genre_bar"), height = "260px")
        )
      )
    )
  )
}

overview_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # KPIs
    output$kpi_steam <- renderUI({
      surge_kpi_card("Steam PC Catalog", format(data$meta$steam_total_count, big.mark = ","), "1997–2025 Digital Titles")
    })

    output$kpi_igdb <- renderUI({
      surge_kpi_card("Cross-Platform", format(data$meta$igdb_total_count, big.mark = ","), "IGDB/HLTB Master Titles")
    })

    output$kpi_platforms <- renderUI({
      surge_kpi_card("Platform Pairs", format(data$meta$platforms_total_count, big.mark = ","), "Across 200 Hardware Platforms")
    })

    output$kpi_sales <- renderUI({
      surge_kpi_card("Historical Sales",
                     paste0(format(round(data$meta$hist_clean_sales, 1), big.mark = ","), " M"),
                     "16,446 Records (1980–2016)")
    })

    output$kpi_bridge <- renderUI({
      surge_kpi_card("Verified Bridge",
                     format(data$meta$bridge_high_conf, big.mark = ","),
                     "High-Confidence Title Links")
    })

    # Overview Chart 1: Platform breakdown
    output$overview_platform_bar <- renderPlotly({
      df <- data$vis1$plat_totals
      p <- plot_ly(
        data = df,
        x = ~reorder(platform_family, total_games),
        y = ~total_games,
        type = "bar",
        marker = list(
          color = c("#38bdf8", "#8b5cf6", "#10b981", "#00f0ff", "#64748b"),
          line = list(color = "rgba(255,255,255,0.15)", width = 0.5)
        ),
        hovertemplate = "<b>%{x}</b><br>Titles: %{y:,}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "", tickangle = -12, tickfont = list(size = 10)),
        yaxis = list(title = "Distinct Titles", gridcolor = "rgba(255,255,255,0.06)", tickfont = list(size = 9)),
        margin = list(t = 10, b = 35, l = 50, r = 15)
      )
      surge_plotly_layout(p, height = 250)
    })

    # Overview Chart 2: Console genre sales
    output$overview_genre_bar <- renderPlotly({
      df <- head(data$vis5$reg_genre, 8)
      p <- plot_ly(
        data = df,
        x = ~Global_Sales,
        y = ~reorder(Genre, Global_Sales),
        type = "bar",
        orientation = "h",
        marker = list(
          color = "#38bdf8",
          line = list(color = "rgba(255,255,255,0.15)", width = 0.5)
        ),
        hovertemplate = "<b>%{y}</b><br>Sales: %{x:.1f}M Units<br>Titles: %{customdata:,}<extra></extra>",
        customdata = ~Game_Count
      ) %>%
      layout(
        xaxis = list(title = "Global Sales (Millions)", gridcolor = "rgba(255,255,255,0.06)", tickfont = list(size = 9)),
        yaxis = list(title = "", tickfont = list(size = 10)),
        margin = list(t = 10, b = 35, l = 90, r = 15)
      )
      surge_plotly_layout(p, height = 250)
    })
  })
}
