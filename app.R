library(shiny)
library(shinyjs)
library(data.table)
library(corrplot)
library(ggplot2)

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
  
  uiOutput("app_content")
)

server <- function(input, output, session) {
  
  # --- Data file path (works both locally and in Docker) -------------------
  data_dir <- "data"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  csv_file <- file.path(data_dir, "KrapfenRating.csv")
  
  # Initialize data file if it doesn't exist
  if (!file.exists(csv_file)) {
    # Try to copy from root if it exists there
    if (file.exists("KrapfenRating.csv")) {
      file.copy("KrapfenRating.csv", csv_file)
    } else {
      stop("KrapfenRating.csv not found in root or data directory. Please add the CSV file.")
    }
  }
  
  # Load data (reactive to refresh after submissions) --------------------
  rating_table <- reactiveVal(fread(csv_file))
  
  # Reactive value to track if user has submitted/exists
  user_authenticated <- reactiveVal(FALSE)
  current_rater <- reactiveVal("")
  is_new_user <- reactiveVal(FALSE)
  new_user_submitted <- reactiveVal(FALSE)
  
  ratings <- reactive({
    rating_table()
  })
  
  # Get krapfen names (all columns except first one which is Rater)
  krapfen_names <- reactive({
    names(ratings())[-1]
  })
  
  # Get list of existing raters
  existing_raters <- reactive({
    ratings()$Rater
  })
  
  # --- Transpose ratings ----------------------------------------------------
  ratings_transposed <- reactive({
    r <- ratings()
    rater_names <- r$Rater
    r[, Rater := NULL]
    rt <- transpose(r)
    setnames(rt, rater_names)
    
    # Ensure all columns are numeric
    for (col in names(rt)) {
      set(rt, j = col, value = as.numeric(rt[[col]]))
    }
    setDT(rt)
    rt
  })
  
  # --- Render app content conditionally ------------------------------------
  output$app_content <- renderUI({
    if (user_authenticated() && is_new_user()) {
      # Show rating page for new users or users who want to rate
      uiOutput("ratings_page")
    } else if (user_authenticated() && !is_new_user()) {
      # Show visualizations after rating is submitted or for existing users viewing results
      sidebarLayout(
        sidebarPanel(
          helpText("Explore correlations and average ratings of different Krapfen."),
          br(),
          actionButton(
            inputId = "back_to_rating_btn",
            label = paste("Back to Rating (", current_rater(), ")", sep = ""),
            class = "btn-secondary",
            style = "width: 100%;"
          ),
          br(),
          br(),
          actionButton(
            inputId = "logout_btn",
            label = "Logout",
            class = "btn-danger",
            style = "width: 100%;"
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Krapfen Similarity",
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
    } else {
      # Show login page
      div(
        style = "padding: 20px;",
        div(
          style = "max-width: 500px; margin: 0 auto; padding: 30px; border: 1px solid #ddd; border-radius: 8px; background: #f9f9f9;",
          h3("Welcome to Krapfen Rating!"),
          p("Enter your name to get started:"),
          textInput(
            inputId = "rater_name",
            label = "Your Name:",
            placeholder = "Enter your name"
          ),
          br(),
          actionButton(
            inputId = "check_name_btn",
            label = "Continue",
            class = "btn-primary",
            style = "width: 100%; padding: 10px;"
          ),
          div(id = "name_message", style = "margin-top: 20px;"),
          hr(),
          h5("Existing Raters:"),
          uiOutput("existing_names_list")
        )
      )
    }
  })
  
  # --- Render existing names list -------------------------------------------
  output$existing_names_list <- renderUI({
    existing <- existing_raters()
    if (length(existing) == 0) {
      p("No raters yet.")
    } else {
      tags$ul(
        lapply(existing, function(name) {
          tags$li(name)
        })
      )
    }
  })
  
  # --- Name check handler ---------------------------------------------------
  observeEvent(input$check_name_btn, {
    name <- trimws(input$rater_name)
    existing <- existing_raters()
    
    if (name == "") {
      shinyjs::runjs("document.getElementById('name_message').innerHTML = '<div class=\"alert alert-danger\">Please enter your name!</div>';");
      return()
    }
    
    current_rater(name)
    
    if (name %in% existing) {
      # Existing user - show rating page to potentially re-rate
      is_new_user(FALSE)
      user_authenticated(TRUE)
    } else {
      # New user - go to rating page
      is_new_user(TRUE)
      user_authenticated(TRUE)
    }
  })
  
  # --- Rating inputs UI -----------------------------------------------------
  output$rating_inputs <- renderUI({
    krapfen <- krapfen_names()
    existing <- existing_raters()
    
    # Show different message based on whether user exists or not
    if (current_rater() %in% existing) {
      intro_text <- p(
        style = "color: #28a745; font-weight: bold;",
        "Welcome back! You've already rated before. Your ratings are saved."
      )
    } else {
      intro_text <- p(
        style = "color: #007bff; font-weight: bold;",
        "Welcome! How would you think these Krapfen taste based on how they look and sound? Please rate each Krapfen on a scale of 1-10."
      )
    }
    
    rating_divs <- lapply(krapfen, function(k) {
      # Use krapfen name directly as image filename with .png extension
      img_path <- paste0(k, ".png")
      
      div(
        style = "margin-bottom: 30px; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
        div(
          style = "display: flex; gap: 20px; align-items: flex-start;",
          div(
            style = "flex-shrink: 0;",
            img(
              src = img_path,
              width = "150px",
              height = "150px",
              style = "border-radius: 5px; object-fit: cover;"
            )
          ),
          div(
            style = "flex-grow: 1;",
            h4(k),
            sliderInput(
              inputId = paste0("rating_", gsub(" ", "_", k)),
              label = "Rating (1-10):",
              min = 1,
              max = 10,
              value = 5,
              step = 1,
              width = "100%"
            )
          )
        )
      )
    })
    
    do.call(tagList, c(list(intro_text), rating_divs))
  })
  
  # --- Ratings page UI (shown after name entry) ----------------------------
  output$ratings_page <- renderUI({
    if (!user_authenticated()) {
      return(NULL)
    }
    
    # Create button list based on whether it's a new user and if they've submitted
    buttons <- list(
      actionButton(
        inputId = "submit_ratings",
        label = "Submit Ratings",
        class = "btn-primary",
        style = "padding: 10px 20px; font-size: 16px;"
      )
    )
    
    # Only show "View Results" button if it's an existing user OR new user has submitted
    if (!is_new_user() || new_user_submitted()) {
      buttons[[2]] <- actionButton(
        inputId = "view_results_btn",
        label = "View Results",
        class = "btn-info",
        style = "padding: 10px 20px; font-size: 16px; margin-left: 10px;"
      )
    }
    
    # Add logout button
    buttons[[length(buttons) + 1]] <- actionButton(
      inputId = "cancel_ratings",
      label = "Logout",
      class = "btn-secondary",
      style = "padding: 10px 20px; font-size: 16px; margin-left: 10px;"
    )
    
    div(
      style = "padding: 20px;",
      h3(paste("Rate Krapfen as", current_rater())),
      uiOutput("rating_inputs"),
      br(),
      do.call(tagList, buttons),
      div(id = "submit_message", style = "margin-top: 20px;")
    )
  })
  
  # --- Submit ratings handler -----------------------------------------------
  observeEvent(input$submit_ratings, {
    rater_name <- current_rater()
    
    # Read current data to get the exact structure
    current_data <- fread(csv_file)
    
    # Get krapfen names from the actual data columns (excluding Rater)
    actual_krapfen <- names(current_data)[-1]
    
    # Collect all ratings in the correct order
    ratings_vector <- sapply(actual_krapfen, function(k) {
      as.numeric(input[[paste0("rating_", gsub(" ", "_", k))]])
    }, USE.NAMES = TRUE)
    
    # Create new row with all columns
    new_row_list <- as.list(c(Rater = rater_name, ratings_vector))
    new_data <- as.data.table(new_row_list)
    
    # Ensure all columns except Rater are numeric
    for (col in names(new_data)) {
      if (col != "Rater") {
        set(new_data, j = col, value = as.numeric(new_data[[col]]))
      }
    }
    
    # Convert all rating columns to numeric in current_data
    for (col in names(current_data)) {
      if (col != "Rater") {
        set(current_data, j = col, value = as.numeric(current_data[[col]]))
      }
    }
    
    # Check if rater already exists
    if (rater_name %in% current_data$Rater) {
      # Update existing row
      current_data[Rater == rater_name] <- new_data
    } else {
      # Append new row
      current_data <- rbind(current_data, new_data, fill = FALSE)
    }
    
    # Write to CSV in data directory
    fwrite(current_data, csv_file, sep = ";")
    
    # Update reactive data table
    rating_table(current_data)
    
    # Mark submission and user as no longer new (for existing users who re-rate)
    new_user_submitted(TRUE)
    is_new_user(FALSE)
  })
  
  # --- Logout handler -------------------------------------------------------
  observeEvent(input$logout_btn, {
    user_authenticated(FALSE)
    is_new_user(FALSE)
    new_user_submitted(FALSE)
    current_rater("")
    updateTextInput(session, "rater_name", value = "")
  })
  
  # --- Cancel ratings handler -------------------------------------------------------
  observeEvent(input$cancel_ratings, {
    user_authenticated(FALSE)
    is_new_user(FALSE)
    new_user_submitted(FALSE)
    current_rater("")
    updateTextInput(session, "rater_name", value = "")
  })
  
  # --- View results handler --------------------------------------------------
  observeEvent(input$view_results_btn, {
    is_new_user(FALSE)
  })
  
  # --- Back to rating handler ------------------------------------------------
  observeEvent(input$back_to_rating_btn, {
    is_new_user(TRUE)
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
    
    # Get all column names except Rater to get krapfen names
    all_cols <- names(r)
    all_krapfen <- all_cols[all_cols != "Rater"]
    
    # Calculate average rating for each krapfen (each row in transposed data)
    rt[, Summed_Rating := rowMeans(.SD, na.rm = TRUE)]
    
    # Assign krapfen names - should match number of rows
    if (nrow(rt) == length(all_krapfen)) {
      rt[, Krapfen := all_krapfen]
    } else {
      # Fallback: use row indices if there's a mismatch
      rt[, Krapfen := paste0("Krapfen_", seq_len(nrow(rt)))]
    }
    
    # Sort by rating
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
