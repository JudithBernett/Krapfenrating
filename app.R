library(shiny)
library(shinyWidgets)
library(data.table)
library(shinyjs)
library(corrplot)
library(ggplot2)
source("posterior.R")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Krapfen Rating Explorer 🍩"),
  
  # --- Feature request badge ---
  tags$div(
    style = "margin-bottom: 15px;",
    tags$a(
      href = "https://github.com/JudithBernett/Krapfenrating",
      target = "_blank",
      tags$img(
        src = "https://img.shields.io/badge/Feature%20Request-GitHub-blue?logo=github",
        alt = "Feature request on GitHub"
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      helpText("Explore correlations and average ratings of different Krapfen."),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Krapfen Similarity",
          
          # Krapfen logo above the plot
          tags$div(
            style = "text-align:center;display:block; margin-left:auto; margin-right:auto; margin-bottom:20px;",
            tags$img(src = "Krapfenlogo.png", height = "300px", style = "max-width: 100%;") 
          ),
          
          shinyWidgets::switchInput(
            inputId = "corr_method_switch",
            label = "Switch Correlation",
            onLabel = "Spearman",
            offLabel = "Pearson",
            value = FALSE
          ),
          plotOutput("corrPlot", height = "1000px")
        ),
        tabPanel(
          "Average Ratings",
          plotOutput("avgPlot", height = "1000px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # --- Load data ------------------------------------------------------------
  rating_table <- fread("KrapfenRating.csv")
  ratings <- reactive({
    rating_table
  })
  
  # --- Transpose ratings ----------------------------------------------------
  ratings_transposed <- reactive({
    r <- ratings()
    rater_names <- r$Rater
    r[, Rater := NULL]
    rt <- transpose(r)
    setnames(rt, rater_names)
    setDT(rt)
    rt
  })
  
  # --- Correlation plot -----------------------------------------------------
  output$corrPlot <- renderPlot({
    rt <- ratings_transposed()
    
    # Check if we have enough raters for correlation
    if (nrow(rt) < 2) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Need at least 2 raters to compute correlations", 
           cex = 1.5, col = "gray40")
      return()
    }
    
    corr_method <- ifelse(input$corr_method_switch, "spearman", "pearson")
    
    corr_matrix <- cor(
      rt,
      method = corr_method,
      use = "pairwise.complete.obs"
    )
    
    # Replace any NA/NaN/Inf values with 0
    corr_matrix[is.na(corr_matrix) | is.infinite(corr_matrix)] <- 0
    
    corrplot(
      corr_matrix,
      order = "hclust",
      tl.col = "black",
      tl.srt = 45,
      col = COL2("PiYG"),
      addCoef.col = "black",
      na.label = "."
    )
  })
  
  # --- Average rating plot --------------------------------------------------
  output$avgPlot <- renderPlot({
    rt <- copy(ratings_transposed())
    r <- copy(ratings())
    rt[, Summed_Rating := rowSums(.SD) / length(colnames(rt))]
    rt[, Krapfen := colnames(r)]
    
    rt <- rt[order(-Summed_Rating)]
    rt[, Krapfen := factor(Krapfen, levels = Krapfen)]

    # compute standard deviation of Summed Rating
    rt[, SD_Rating := apply(.SD, 1, sd, na.rm = TRUE), .SDcols= colnames(ratings_transposed())]
    
    ggplot(rt, aes(x = Krapfen, y = Summed_Rating, color=Krapfen)) +
      geom_point(size = 5) +
      # add errorbars
      geom_errorbar(aes(ymin = Summed_Rating - SD_Rating,
                        ymax = Summed_Rating + SD_Rating),
                    width = 0.2) +
      theme_minimal() +
      ylim(1, 10) +
      # add y axis ticks at every integer
      scale_y_continuous(breaks = seq(1, 10, by = 1)) +
      labs(
        title = "Average Krapfen Ratings",
        x = "Krapfen",
        y = "Average Points"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            text = element_text(size = 16))
  })
  
  # --- Render posterior plot ---
  output$posteriorPlot <- renderPlot({
    req(input$selected_krapfen)
    
    krapfen <- input$selected_krapfen
    
    # survey data for selected krapfen
    survey_vec <- rating_table[[krapfen]]
    
    # expert data
    expert_vec <- as.numeric(expert_data[[krapfen]])
    
    plot_comparison(survey_vec, expert_vec)
  })
}

shinyApp(ui, server)
