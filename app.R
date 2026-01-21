library(shiny)
library(data.table)
library(corrplot)
library(ggplot2)

ui <- fluidPage(
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
    corr_method <- ifelse(input$corr_method_switch, "spearman", "pearson")
    
    corr_matrix <- cor(
      rt,
      method = corr_method,
      use = "pairwise.complete.obs"
    )
    
    corrplot(
      corr_matrix,
      order = "hclust",
      tl.col = "black",
      tl.srt = 45,
      col = COL2("PiYG"),
      addCoef.col = "black"
    )
  })
  
  # --- Average rating plot --------------------------------------------------
  output$avgPlot <- renderPlot({
    rt <- copy(ratings_transposed())
    r <- copy(ratings())
    rt[, Summed_Rating := rowSums(.SD) / .N]
    rt[, Krapfen := colnames(r)]
    
    rt <- rt[order(-Summed_Rating)]
    rt[, Krapfen := factor(Krapfen, levels = Krapfen)]
    
    ggplot(rt, aes(x = Krapfen, y = Summed_Rating)) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(
        title = "Average Krapfen Ratings",
        x = "Krapfen",
        y = "Average Points"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            text = element_text(size = 16))
  })
}

shinyApp(ui, server)
