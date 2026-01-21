library(shiny)
library(data.table)
library(corrplot)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Krapfen Rating Explorer 🍩"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore correlations and average ratings of different Krapfen.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Krapfen Similarity",
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
    corr_matrix <- cor(rt, method = "pearson")
    
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
