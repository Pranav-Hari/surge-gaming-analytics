# ==============================================================================
# SURGE — Module: Visualization 6 (Ecosystem Associations)
# Multi-Metric Spearman Correlation Matrix with Seriation and Significance Glyphs
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(data.table)

vis6_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row g-3 mb-3",
      # Filter Controls Sidebar
      div(
        class = "col-lg-3",
        surge_card(
          title = "Correlation Controls",
          subtitle = "Dynamic subsetting & variable diagnostics",
          content = tagList(
            selectInput(
              ns("genre_filter"),
              tags$b("Catalog Domain / Genre:"),
              choices = c(
                "Full Steam Catalog (N = 58,595)" = "full",
                "Action Titles" = "Action",
                "Role-Playing (RPG)" = "RPG",
                "Strategy Titles" = "Strategy",
                "Indie Releases" = "Indie"
              ),
              selected = "full"
            ),
            selectInput(
              ns("color_palette"),
              tags$b("Diverging Palette:"),
              choices = c("Cyan-Obsidian-Rose" = "coolwarm", "Red-White-Blue" = "RdBu", "Purple-Green" = "PRGn", "Amber-Cyan" = "Spectral"),
              selected = "coolwarm"
            ),
            hr(style = "border-color: var(--border-subtle);"),
            tags$ul(
              class = "small text-muted ps-3 mb-0",
              tags$li("Spearman rank correlation handles extreme skew in player and review counts."),
              tags$li("Covers 58,595 titles with complete records across all 8 metrics."),
              tags$li("Hierarchical clustering groups metrics with similar association patterns.")
            )
          )
        ),
        # KPI Stats Card
        surge_card(
          title = "Key Empirical Associations",
          content = tagList(
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Reviews & Peak Players:"),
                tags$b(style = "color: var(--brand-gold);", "Strong Link (+0.72)")
            ),
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Playtime & Active Players:"),
                tags$b(style = "color: var(--brand-gold);", "Moderate Link (+0.44)")
            ),
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Price & Player Sentiment:"),
                tags$b(style = "color: var(--neon-amber);", "No Link (+0.08)")
            ),
            div(class = "d-flex justify-content-between mb-2",
                span(class = "text-muted small", "Discount & Sentiment:"),
                tags$b(style = "color: var(--neon-amber);", "No Link (-0.01)")
            ),
            div(class = "d-flex justify-content-between pt-1 border-top border-secondary",
                span(class = "text-muted small", "Games Analyzed:"),
                tags$b(textOutput(ns("n_cases_txt"), inline = TRUE))
            ),
            p(
              class = "small text-muted mt-2 mb-0",
              "Review volumes closely reflect concurrent player numbers, but higher prices and discounts show no connection to player satisfaction."
            ),
            p(
              class = "small text-muted mt-1 mb-0",
              tags$i("These patterns describe observed associations across games, not proof of cause and effect.")
            )
          )
        )
      ),

      # Main Correlation Heatmap
      div(
        class = "col-lg-9",
        surge_card(
          title = "Ecosystem Associations (Spearman Rank Correlation)",
          subtitle = "Rank correlation across pricing, engagement, playtime, and review metrics",
          content = plotlyOutput(ns("corr_heatmap"), height = "560px"),
          takeaway = "Total review volume correlates strongly with peak concurrent players (+0.72), and playtime aligns moderately with concurrency (+0.44). Game price and discount depth show virtually no link to player satisfaction.",
          limitation = "Correlation does not mean causation. Missing and zero playtime or concurrency values reflect public profile privacy settings and do not prove zero actual play."
        )
      )
    ),

    # Association Inspector
    div(
      class = "row",
      div(
        class = "col-12",
        surge_card(
          title = "Metric Association Inspector & Significance Summary",
          subtitle = "Pairwise Spearman coefficients with statistical significance levels",
          content = DTOutput(ns("pairwise_table"))
        )
      )
    )
  )
}

vis6_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Active correlation object
    active_corr <- reactive({
      if (input$genre_filter == "full" || is.null(data$vis6$genre_corrs[[input$genre_filter]])) {
        data$vis6$corr_full
      } else {
        data$vis6$genre_corrs[[input$genre_filter]]
      }
    })

    output$n_cases_txt <- renderText({
      if (input$genre_filter == "full") {
        format(data$vis6$N_complete, big.mark = ",")
      } else {
        corr_obj <- data$vis6$genre_corrs[[input$genre_filter]]
        format(corr_obj$N, big.mark = ",")
      }
    })

    # Heatmap Plot
    output$corr_heatmap <- renderPlotly({
      corr_obj <- active_corr()
      cor_mat <- corr_obj$cor
      p_mat   <- corr_obj$p
      ordered_cols <- corr_obj$cols_ordered
      labels <- data$vis6$v6_labels[ordered_cols]

      n <- length(ordered_cols)
      hover_text <- matrix("", n, n)
      annot_text <- matrix("", n, n)

      for (i in 1:n) {
        for (j in 1:n) {
          r_val <- cor_mat[i, j]
          p_val <- p_mat[i, j]
          sig_stars <- if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else ""

          annot_text[i, j] <- sprintf("%.2f%s", r_val, sig_stars)
          hover_text[i, j] <- paste0(
            "<b>Pair:</b> ", labels[i], " ↔ ", labels[j], "<br>",
            "<b>Spearman rs:</b> ", sprintf("%.3f", r_val), "<br>",
            "<b>p-value:</b> ", if (p_val < 0.0001) "< 0.0001" else sprintf("%.4f", p_val), "<br>",
            "<b>Significance:</b> ", if (sig_stars != "") paste("Significant (", sig_stars, ")") else "Not Significant"
          )
        }
      }

      # Dynamic Diverging color palette configuration
      palette_map <- list(
        "coolwarm" = list(c(0.0, "#f43f5e"), c(0.5, "#0b101b"), c(1.0, "#00f0ff")),
        "RdBu"     = list(c(0.0, "#ef4444"), c(0.5, "#1e293b"), c(1.0, "#3b82f6")),
        "PRGn"     = list(c(0.0, "#a855f7"), c(0.5, "#0f172a"), c(1.0, "#10b981")),
        "Spectral" = list(c(0.0, "#f59e0b"), c(0.5, "#0f172a"), c(1.0, "#06b6d4"))
      )
      chosen_palette <- if (!is.null(input$color_palette) && input$color_palette %in% names(palette_map)) {
        palette_map[[input$color_palette]]
      } else {
        palette_map[["coolwarm"]]
      }

      p <- plot_ly(
        x = unname(labels),
        y = unname(labels),
        z = cor_mat,
        type = "heatmap",
        colorscale = chosen_palette,
        zmin = -1.0,
        zmax = 1.0,
        text = hover_text,
        hoverinfo = "text",
        colorbar = list(
          title = list(text = "Spearman rs", font = list(color = "#ffffff", size = 11)),
          tickvals = c(-1, -0.5, 0, 0.5, 1),
          tickfont = list(color = "#94a3b8", size = 10)
        )
      )

      # Add cell annotations
      annotations_list <- list()
      for (i in 1:n) {
        for (j in 1:n) {
          annotations_list[[length(annotations_list) + 1]] <- list(
            x = labels[j],
            y = labels[i],
            text = annot_text[i, j],
            showarrow = FALSE,
            font = list(
              color = if (abs(cor_mat[i, j]) > 0.45) "#000000" else "#ffffff",
              size = 10
            )
          )
        }
      }

      p <- p %>% layout(
        xaxis = list(title = "", tickangle = -25, tickfont = list(size = 10)),
        yaxis = list(title = "", tickfont = list(size = 10)),
        annotations = annotations_list,
        margin = list(l = 150, r = 20, t = 20, b = 100)
      )

      surge_plotly_layout(p, height = 540)
    })

    # Pairwise Table
    output$pairwise_table <- renderDT({
      corr_obj <- active_corr()
      cor_mat <- corr_obj$cor
      p_mat   <- corr_obj$p
      ordered_cols <- corr_obj$cols_ordered
      labels <- data$vis6$v6_labels[ordered_cols]

      rows <- list()
      n <- length(ordered_cols)
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          r_val <- cor_mat[i, j]
          p_val <- p_mat[i, j]
          sig_level <- if (p_val < 0.001) "p < 0.001 (***)" else if (p_val < 0.01) "p < 0.01 (**)" else if (p_val < 0.05) "p < 0.05 (*)" else "Not Sig"
          strength <- if (abs(r_val) > 0.6) "Strong" else if (abs(r_val) > 0.3) "Moderate" else if (abs(r_val) > 0.1) "Weak" else "Negligible"
          direction <- if (r_val > 0) "Positive" else "Negative"

          rows[[length(rows) + 1]] <- data.frame(
            `Variable A` = labels[i],
            `Variable B` = labels[j],
            `Spearman rs` = sprintf("%.3f", r_val),
            `Direction` = direction,
            `Strength` = strength,
            `Significance` = sig_level,
            check.names = FALSE
          )
        }
      }

      dt <- rbindlist(rows)
      surge_datatable(
        dt,
        pageLength = 10,
        enable_page_size = TRUE,
        options = list(
          order = list(list(2, "desc"))
        )
      )
    })
  })
}
