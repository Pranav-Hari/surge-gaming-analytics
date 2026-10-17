# ==============================================================================
# SURGE — Module: Visualization 7 (Gaming Archetypes)
# Unsupervised PCA & K-Means Clustering of Modern PC Games (Dual-Panel + Explorer)
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(data.table)

vis7_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row g-3 mb-3",
      # Controls Sidebar
      div(
        class = "col-lg-3",
        surge_card(
          title = "Archetype Controls",
          subtitle = "PCA biplot & cluster isolation controls",
          content = tagList(
            checkboxGroupInput(
              ns("cluster_select"),
              tags$b("Isolate Game Archetypes:"),
              choices = c(
                "Established Commercial / High-Engagement" = "Established Commercial / High-Engagement",
                "Engaged Mid-Tier Indies / Community Favorites" = "Engaged Mid-Tier Indies / Community Favorites",
                "Achievement-Enabled Modest Indie Releases" = "Achievement-Enabled Modest Indie Releases",
                "Minimalist / Budget Barebone Catalog" = "Minimalist / Budget Barebone Catalog"
              ),
              selected = c(
                "Established Commercial / High-Engagement",
                "Engaged Mid-Tier Indies / Community Favorites",
                "Achievement-Enabled Modest Indie Releases",
                "Minimalist / Budget Barebone Catalog"
              )
            ),
            checkboxInput(
              ns("show_loadings"),
              tags$b("Overlay PCA Loading Vectors (Eigenvectors)"),
              value = TRUE
            ),
            selectizeInput(
              ns("search_game"),
              tags$b("Locate Benchmark Game:"),
              choices = NULL,
              selected = NULL,
              options = list(placeholder = "Search (e.g. Cyberpunk 2077, Terraria)...", maxOptions = 50)
            ),
            uiOutput(ns("selected_game_summary")),
            hr(style = "border-color: var(--border-subtle);"),
            tags$ul(
              class = "small text-muted ps-3 mb-0",
              tags$li("PC1 captures commercial scale and reach (39.5% variance); PC2 reflects player acclaim (14.3%)."),
              tags$li("K = 4 clusters separate games into meaningful market segments rather than a simple binary split."),
              tags$li("Includes 38,032 games meeting the 30-review threshold.")
            )
          )
        ),
        # Variance Decomposition Card
        surge_card(
          title = "PCA Variance Decomposition",
          content = tagList(
            div(class = "d-flex justify-content-between mb-1",
                span(class = "text-muted small", "PC1 (Scale & Reach):"),
                tags$b(style = "color: var(--brand-gold);", "39.50% Variance")
            ),
            div(class = "d-flex justify-content-between mb-1",
                span(class = "text-muted small", "PC2 (Acclaim & Sentiment):"),
                tags$b(style = "color: var(--neon-violet);", "14.28% Variance")
            ),
            div(class = "d-flex justify-content-between mb-1",
                span(class = "text-muted small", "PC3 (Progression & DLC):"),
                tags$b("14.12% Variance")
            ),
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "PC4 (Base Price):"),
                tags$b("10.68% Variance")
            ),
            div(class = "d-flex justify-content-between pt-1 border-top border-secondary",
                span(class = "text-muted small", "Cumulative (PC1–PC4):"),
                tags$b(style = "color: var(--neon-emerald);", "78.58%")
            )
          )
        )
      ),

      # Main Dual-Panel View
      div(
        class = "col-lg-9",
        # Panel A: PCA Biplot
        surge_card(
          title = "PCA Biplot Projection (PC1 vs PC2)",
          subtitle = "Two-dimensional projection of games based on commercial scale, player engagement, and acclaim",
          content = plotlyOutput(ns("pca_biplot"), height = "440px")
        ),
        # Panel B: Cluster Profiles
        surge_card(
          title = "Empirical Archetype Profiles (Original-Scale Medians)",
          subtitle = "Median attributes of the four discovered game clusters",
          content = plotlyOutput(ns("cluster_profile_chart"), height = "280px"),
          takeaway = "PC games cluster into four empirical groups: Established Commercial Releases (9.1%), Engaged Mid-Tier Indies (13.5%), Achievement-Enabled Indies (44.8%), and Minimalist Budget Catalog (32.6%).",
          limitation = "Cluster boundaries are sensitive to zero-recorded engagement values. K = 4 was selected for interpretability and is not a natural biological classification."
        )
      )
    ),

    # Empirical Cluster Benchmark Table
    div(
      class = "row",
      div(
        class = "col-12",
        surge_card(
          title = "Empirical Cluster Benchmark Table",
          subtitle = "Detailed median & quantile attributes for each cluster (N = 38,032)",
          content = DTOutput(ns("cluster_table"))
        )
      )
    )
  )
}

vis7_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Initialize search options with diverse representative benchmarks across all 4 clusters
    observe({
      curated_benchmarks <- c(
        # Minimalist / Budget Barebone Catalog (Cluster 1)
        "Crysis 2 - Maximum Edition", "Deus Ex: Game of the Year Edition",
        "Five Nights at Freddy's 4", "Grimm's Hollow", "Need For Speed: Hot Pursuit",
        "Pool 2D - Poolians", "Shadowrun Returns", "The Expendabros",

        # Engaged Mid-Tier Indies / Community Favorites (Cluster 2)
        "Blood and Bacon", "Bright Memory", "Bully: Scholarship Edition",
        "Emily is Away", "Five Nights at Freddy's", "LIMBO", "Sven Co-op",
        "The Room", "The Stanley Parable",

        # Achievement-Enabled Modest Indie Releases (Cluster 3)
        "Archimedes: Eureka!", "Deadlight", "Double Action: Boogaloo",
        "Montaro", "ORION: Prelude", "Super Hexagon", "Transmissions: Element 120",
        "Walking Simulator", "Yet Another Zombie Defense",

        # Established Commercial / High-Engagement (Cluster 4)
        "Baldur's Gate 3", "Celeste", "Counter-Strike 2", "Cyberpunk 2077",
        "Deep Rock Galactic", "Dota 2", "ELDEN RING", "Euro Truck Simulator 2",
        "Grand Theft Auto V Legacy", "Hades", "Hollow Knight", "Phasmophobia",
        "Portal 2", "Red Dead Redemption 2", "Rust", "Slay the Spire",
        "Stardew Valley", "Team Fortress 2", "Terraria", "The Witcher 3: Wild Hunt",
        "Valheim", "Vampire Survivors"
      )
      valid_names <- sort(intersect(curated_benchmarks, data$vis7$full_search_lookup$name))

      updateSelectizeInput(
        session, "search_game",
        choices = c("None", valid_names),
        selected = "None",
        server = TRUE
      )
    })

    # Filter points by cluster isolation
    active_points <- reactive({
      req(input$cluster_select)
      df <- copy(data$vis7$sampled_points)
      df <- df[cluster_label %in% input$cluster_select]
      df
    })

    # PCA Biplot
    output$pca_biplot <- renderPlotly({
      df <- active_points()
      validate(need(nrow(df) > 0, "Please select at least one cluster to display."))

      # Cluster colors
      cluster_colors <- c(
        "Established Commercial / High-Engagement"     = "#00f0ff", # Cyan
        "Engaged Mid-Tier Indies / Community Favorites" = "#10b981", # Emerald
        "Achievement-Enabled Modest Indie Releases"     = "#a855f7", # Violet
        "Minimalist / Budget Barebone Catalog"          = "#64748b"  # Slate
      )

      p <- plot_ly()

      # Add scatter traces for each selected cluster
      for (cl in unique(df$cluster_label)) {
        sub <- df[cluster_label == cl]
        c_val <- if (cl %in% names(cluster_colors)) cluster_colors[[cl]] else "#38bdf8"

        p <- p %>% add_trace(
          data = sub,
          x = ~PC1,
          y = ~PC2,
          type = "scatter",
          mode = "markers",
          name = cl,
          marker = list(
            size = ~pmax(4, log10(num_reviews_total + 1) * 2.2),
            color = c_val,
            opacity = 0.65,
            line = list(color = "#ffffff", width = 0.3)
          ),
          text = ~paste0(
            "<b>", name, "</b><br>",
            "Archetype: ", cluster_label, "<br>",
            "Price: $", sprintf("%.2f", price), "<br>",
            "Reviews: ", format(num_reviews_total, big.mark = ","), "<br>",
            "Positive: ", pct_pos_total, "%<br>",
            "Peak CCU: ", format(peak_ccu, big.mark = ","), "<br>",
            "PC1: ", round(PC1, 2), " | PC2: ", round(PC2, 2)
          ),
          hoverinfo = "text"
        )
      }

      # Highlight searched benchmark game if selected
      if (!is.null(input$search_game) && input$search_game != "None") {
        searched <- data$vis7$full_search_lookup[name == input$search_game]
        if (nrow(searched) > 0) {
          p <- p %>% add_trace(
            data = searched[1],
            x = ~PC1,
            y = ~PC2,
            type = "scatter",
            mode = "markers+text",
            name = paste("Search:", input$search_game),
            text = ~name,
            textposition = "top right",
            textfont = list(color = "#ffffff", size = 12, family = "'Outfit', sans-serif"),
            marker = list(
              size = 14,
              color = "#f59e0b",
              symbol = "star",
              line = list(color = "#ffffff", width = 1.5)
            ),
            hoverinfo = "text"
          )
        }
      }

      # Overlay loading vectors if enabled
      if (input$show_loadings) {
        loadings <- data$vis7$pca_loadings
        scale_factor <- 4.2 # scaling for visibility in PC space

        for (i in 1:nrow(loadings)) {
          var_name <- rownames(loadings)[i]
          vx <- loadings[i, 1] * scale_factor
          vy <- loadings[i, 2] * scale_factor

          p <- p %>% add_segments(
            x = 0, y = 0, xend = vx, yend = vy,
            line = list(color = "#ffffff", width = 1.2, dash = "dot"),
            showlegend = FALSE,
            hoverinfo = "none"
          ) %>% add_annotations(
            x = vx * 1.12, y = vy * 1.12,
            text = var_name,
            showarrow = FALSE,
            bgcolor = "rgba(13, 18, 29, 0.8)",
            bordercolor = "rgba(0, 240, 255, 0.3)",
            borderwidth = 1,
            borderpad = 3,
            font = list(color = "#ffffff", size = 10, family = "'JetBrains Mono', monospace")
          )
        }
      }

      p <- p %>% layout(
        xaxis = list(title = "PC1: Commercial Scale & Community Reach (39.50% Variance)", zeroline = TRUE),
        yaxis = list(title = "PC2: Player Acclaim & Sentiment (14.28% Variance)", zeroline = TRUE),
        legend = list(orientation = "h", x = 0, y = 1.12)
      )

      surge_plotly_layout(p, height = 420)
    })

    # Cluster Profile Chart (Panel B)
    output$cluster_profile_chart <- renderPlotly({
      req(input$cluster_select)
      profiles <- copy(data$vis7$cluster_profiles)
      profiles <- profiles[cluster_label %in% input$cluster_select]

      p <- plot_ly(data = profiles) %>%
        add_trace(
          x = ~cluster_label,
          y = ~Price,
          type = "bar",
          name = "Median Price ($)",
          marker = list(color = "#38bdf8")
        ) %>%
        add_trace(
          x = ~cluster_label,
          y = ~Sentiment,
          type = "bar",
          name = "Median % Positive",
          marker = list(color = "#10b981")
        ) %>%
        add_trace(
          x = ~cluster_label,
          y = ~Achievements,
          type = "bar",
          name = "Median Achievements",
          marker = list(color = "#a855f7")
        ) %>%
        layout(
          barmode = "group",
          xaxis = list(title = "", tickangle = -12, tickfont = list(size = 9)),
          yaxis = list(title = "Metric Value"),
          legend = list(orientation = "h", x = 0, y = 1.15)
        )

      surge_plotly_layout(p, height = 260)
    })

    # Selected Game Inline Summary for Benchmark Selector
    output$selected_game_summary <- renderUI({
      game_name <- input$search_game
      if (is.null(game_name) || game_name == "None") {
        return(div(class = "small text-muted mt-1", tags$i("Select a benchmark game to highlight its position on the biplot.")))
      }
      match_game <- data$vis7$full_search_lookup[name == game_name][1]
      if (nrow(match_game) == 0) return(NULL)

      div(
        class = "p-2 rounded mt-2 border",
        style = "background: rgba(15, 23, 42, 0.7); border-color: var(--border-subtle) !important;",
        div(class = "d-flex justify-content-between align-items-center mb-1",
            tags$b(style = "color: #f59e0b; font-size: 0.85rem;", match_game$name),
            span(class = "badge bg-secondary", style = "font-size: 0.65rem;", paste0("AppID: ", match_game$steam_appid))
        ),
        div(class = "small text-muted mb-1", tags$i(match_game$cluster_label)),
        div(class = "d-flex justify-content-between small text-muted",
            span("Retail Price:"), tags$b(style = "color: #e2e8f0;", paste0("$", sprintf("%.2f", match_game$price)))
        ),
        div(class = "d-flex justify-content-between small text-muted",
            span("Positive Reviews:"), tags$b(style = "color: #e2e8f0;", paste0(match_game$pct_pos_total, "%"))
        ),
        div(class = "d-flex justify-content-between small text-muted",
            span("Review Count:"), tags$b(style = "color: #e2e8f0;", format(match_game$num_reviews_total, big.mark = ","))
        ),
        div(class = "d-flex justify-content-between small text-muted",
            span("Peak Concurrent:"), tags$b(style = "color: #e2e8f0;", format(match_game$peak_ccu, big.mark = ","))
        ),
        div(class = "d-flex justify-content-between small text-muted",
            span("Biplot Coords:"), tags$b(style = "color: var(--brand-gold);", paste0("PC1=", round(match_game$PC1, 2), " | PC2=", round(match_game$PC2, 2)))
        )
      )
    })

    # Cluster benchmark table
    output$cluster_table <- renderDT({
      profiles <- copy(data$vis7$cluster_profiles)
      dt_display <- data.frame(
        `Archetype Label` = profiles$cluster_label,
        `Sample N (%)` = paste0(format(profiles$N, big.mark = ","), " (", profiles$pct_catalog, "%)"),
        `Median Price` = paste0("$", sprintf("%.2f", profiles$Price)),
        `Median Reviews` = format(profiles$Reviews, big.mark = ","),
        `Median Peak CCU` = format(profiles$Peak_CCU, big.mark = ","),
        `Median Playtime` = paste0(profiles$Playtime, " min"),
        `Median Achievements` = profiles$Achievements,
        `Median DLC` = profiles$DLC_Count,
        `Median % Pos` = paste0(profiles$Sentiment, "%"),
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
