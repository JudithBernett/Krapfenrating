library(shiny)
library(shinyjs)
library(shinyWidgets)
library(data.table)
library(corrplot)
library(ggplot2)
library(png)
library(grid)

source("posterior.R")
source("utils.R")

ui <- fluidPage(
  useShinyjs(),
  
  # Custom CSS for modern styling
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      
      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        background: linear-gradient(135deg, #1e3a5f 0%, #2c5f8d 100%);
        min-height: 100vh;
        padding: 20px;
      }
      
      .container-fluid {
        max-width: 1400px;
        margin: 0 auto;
      }
      
      h2, h3, h4, h5 {
        font-weight: 600;
        color: #2d3748;
      }
      
      .btn {
        border-radius: 8px;
        font-weight: 500;
        transition: all 0.3s ease;
        border: none;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #2c7da0 0%, #014f86 100%);
      }
      
      .btn-primary:hover {
        background: linear-gradient(135deg, #236a87 0%, #013d6a 100%);
      }
      
      .btn-secondary {
        background: #718096;
      }
      
      .btn-secondary:hover {
        background: #4a5568;
      }
      
      .btn-success {
        background: #2a9d8f;
      }
      
      .btn-success:hover {
        background: #21867a;
      }
      
      .btn-danger {
        background: #e76f51;
      }
      
      .btn-danger:hover {
        background: #d4603f;
      }
      
      .btn-info {
        background: #457b9d;
      }
      
      .btn-info:hover {
        background: #3a6785;
      }
      
      .well {
        background: white;
        border: none;
        border-radius: 12px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.07);
      }
      
      .nav-tabs {
        border-bottom: 2px solid #e2e8f0;
      }
      
      .nav-tabs > li > a {
        border-radius: 8px 8px 0 0;
        color: #4a5568;
        font-weight: 500;
      }
      
      .nav-tabs > li.active > a {
        background: linear-gradient(135deg, #2c7da0 0%, #014f86 100%);
        color: white;
        border: none;
      }
      
      .selectize-input {
        border-radius: 8px;
        border: 2px solid #e2e8f0;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
        transition: all 0.2s ease;
      }
      
      .selectize-input:focus {
        border-color: #2c7da0;
        box-shadow: 0 0 0 3px rgba(44, 125, 160, 0.1);
      }
      
      .form-control {
        border-radius: 8px;
        border: 2px solid #e2e8f0;
        transition: all 0.2s ease;
      }
      
      .form-control:focus {
        border-color: #2c7da0;
        box-shadow: 0 0 0 3px rgba(44, 125, 160, 0.1);
      }
      
      .alert {
        border-radius: 8px;
        border: none;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .alert-success {
        background: #d4edda;
        color: #155724;
      }
      
      .alert-danger {
        background: #f8d7da;
        color: #721c24;
      }
      
      .header-card {
        background: white;
        border-radius: 16px;
        padding: 30px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      
      .stats-card {
        background: white;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.07);
      }
      
      .rating-card {
        background: white;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        transition: all 0.3s ease;
      }
      
      .rating-card:hover {
        box-shadow: 0 8px 16px rgba(0,0,0,0.12);
        transform: translateY(-2px);
      }
      
      .irs-bar {
        background: linear-gradient(135deg, #2c7da0 0%, #014f86 100%);
      }
      
      .irs-from, .irs-to, .irs-single {
        background: #2c7da0;
      }
    "))
  ),
  
  # Header with badge
  div(
    class = "header-card",
    div(
      style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap;",
      div(
        h2(style = "margin: 0; color: #2c7da0;", "🍩 ", span("Krapfen Rating Explorer", style = "background: linear-gradient(135deg, #2c7da0 0%, #014f86 100%); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text;"))
      ),
      div(
        tags$a(
          href = "https://github.com/JudithBernett/Krapfenrating",
          target = "_blank",
          tags$img(
            src = "https://img.shields.io/badge/Feature%20Request-GitHub-blue?logo=github",
            alt = "Feature request on GitHub"
          )
        )
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
  background_file <- file.path(data_dir, "KrapfenRating.csv")
  rating_file <- file.path(data_dir, "real_ratings.csv")
  
  # Load data (reactive to refresh after submissions) --------------------
  background_data <- reactiveVal(fread(background_file))
  rating_data <- reactiveVal(fread(rating_file))
  
  # Reactive value to track if user has submitted/exists
  user_authenticated <- reactiveVal(FALSE)
  current_rater <- reactiveVal("")
  is_new_user <- reactiveVal(TRUE)
  new_user_submitted <- reactiveVal(FALSE)
  
  # which view of the app is active?
  app_mode <- reactiveVal("login_page_ui")  
  # values: "login_page_ui", "view", "rate_all", "rate_single"
    
  # Get krapfen names (all columns except first one which is Rater)
  krapfen_names <- reactive({
    names(background_data())[-1]
  })
  
  expert_rated_krapfen <- reactive({
    r <- copy(rating_data())
    rated <- names(r)[colSums(!is.na(r)) > 0]
    rated <- rated[rated != "Rater"]
    return(rated)
  })
  
  # Get list of existing raters
  existing_raters <- reactive({
    background_data()$Rater
  })
  
  # --- Render app content conditionally ------------------------------------
  output$app_content <- renderUI({
    if (!user_authenticated()) {
      return(uiOutput("login_page_ui"))
    }
    
    if (app_mode() == "rate_all") {
      # Show rating page for new users or users who want to rate
      return(uiOutput("ratings_page"))
    }
    
    if (app_mode() == "rate_single") {
      return(uiOutput("single_rating_page"))
    } 
    
    return(uiOutput("view_page_ui"))
    
  })
  
  output$login_page_ui <- renderUI({
    # Show login page
    existing <- sort(existing_raters())
    
    # Calculate stats
    num_raters <- length(existing)
    num_krapfen <- length(krapfen_names())
    total_ratings <- sum(!is.na(background_data()[, -1]))
    num_expert_ratings <- sum(!is.na(rating_data()[, -1]))
    
    div(
      style = "padding: 20px;",
      div(
        class = "stats-card",
        style = "max-width: 550px; margin: 0 auto; padding: 40px;",
        h3("Welcome to Krapfen Rating! 👋", style = "text-align: center; color: #2d3748; margin-bottom: 25px;"),
        p("Select your name from the list or type a new name:", style = "color: #4a5568; text-align: center; margin-bottom: 20px;"),
        selectizeInput(
          inputId = "rater_name",
          label = "Your Name:",
          choices = existing,
          selected = NULL,
          options = list(
            create = TRUE,
            placeholder = "Select or type your name..."
          )
        ),
        br(),
        actionButton(
          inputId = "check_name_btn",
          label = "Continue →",
          class = "btn-primary",
          style = "width: 100%; padding: 12px; font-size: 16px;"
        ),
        div(id = "name_message", style = "margin-top: 20px;")
      ),
      br(), br(),
      div(
        class = "stats-card",
        style = "max-width: 550px; margin: 0 auto; padding: 30px;",
        h4("📊 Current Statistics", style = "margin-top: 0; color: #2d3748; margin-bottom: 20px; text-align: center;"),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
          div(
            style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #2c7da015 0%, #014f8615 100%); border-radius: 10px;",
            div(style = "font-size: 32px; font-weight: 700; color: #2c7da0;", num_raters),
            div(style = "font-size: 14px; color: #718096; margin-top: 5px;", "Total Raters")
          ),
          div(
            style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #2c7da015 0%, #014f8615 100%); border-radius: 10px;",
            div(style = "font-size: 32px; font-weight: 700; color: #014f86;", num_krapfen),
            div(style = "font-size: 14px; color: #718096; margin-top: 5px;", "Krapfen Types")
          ),
          div(
            style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #2c7da015 0%, #014f8615 100%); border-radius: 10px;",
            div(style = "font-size: 32px; font-weight: 700; color: #2c7da0;", total_ratings),
            div(style = "font-size: 14px; color: #718096; margin-top: 5px;", "Background Ratings")
          ),
          div(
            style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #2c7da015 0%, #014f8615 100%); border-radius: 10px;",
            div(style = "font-size: 32px; font-weight: 700; color: #014f86;", num_expert_ratings),
            div(style = "font-size: 14px; color: #718096; margin-top: 5px;", "Expert Ratings")
          )
        )
      )
    )
  })
  
  output$view_page_ui <- renderUI({
    # Show visualizations after rating is submitted or for existing users viewing results
    sidebarLayout(
      sidebarPanel(
        width = 2,
        style = "background: white; border-radius: 12px; padding: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.07);",
        div(
          style = "text-align: center; margin-bottom: 20px; padding: 15px; background: linear-gradient(135deg, #2c7da015 0%, #014f8615 100%); border-radius: 8px;",
          div(style = "font-size: 24px; margin-bottom: 5px;", "👤"),
          div(style = "font-weight: 600; color: #2d3748;", current_rater())
        ),
        p("Explore correlations and average ratings of different Krapfen.", style = "color: #718096; font-size: 14px; text-align: center;"),
        hr(style = "border-color: #e2e8f0;"),
        actionButton(
          inputId = "back_to_rating_btn",
          label = "📝 Rate All Krapfen",
          class = "btn-secondary",
          style = "width: 100%; margin-bottom: 10px; padding: 10px;"
        ),
        actionButton(
          inputId = "rate_single_btn",
          label = "🍩 Rate Single Krapfen",
          class = "btn-success",
          style = "width: 100%; margin-bottom: 10px; padding: 10px;"
        ),
        hr(style = "border-color: #e2e8f0;"),
        actionButton(
          inputId = "logout_btn",
          label = "🚪 Logout",
          class = "btn-danger",
          style = "width: 100%; padding: 10px;"
        )
      ),
      mainPanel(
        style = "background: white; border-radius: 12px; padding: 25px; box-shadow: 0 4px 6px rgba(0,0,0,0.07);",
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
          ),
          tabPanel(
            "Posterior Distribution",
            
            # Picker to select Krapfen
            pickerInput(
              inputId = "selected_krapfen",
              label = "Select Krapfen:",
              choices = expert_rated_krapfen(),
              selected = expert_rated_krapfen()[1],
              options = list(`live-search` = TRUE)
            ),
            
            # Toggle correlation method (optional, can skip here)
            
            plotOutput("posteriorPlot", height = "500px")
          )
        )
      )
    )
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
      # Existing user - navigate to view page (plots)
      is_new_user(FALSE)
      user_authenticated(TRUE)
      app_mode("view")
    } else {
      # New user - navigate to rating page
      is_new_user(TRUE)
      user_authenticated(TRUE)
      app_mode("rate_all")
    }
  })
  
  # --- Rating inputs UI -----------------------------------------------------
  output$rating_inputs <- renderUI({
    krapfen <- krapfen_names()
    existing <- existing_raters()
    
    # Show different message based on whether user exists or not
    if (current_rater() %in% existing) {
      intro_text <- div(
        style = "padding: 20px; background: #c6f6d5; border-radius: 12px; margin-bottom: 25px; text-align: center;",
        p(style = "color: #22543d; font-weight: 600; margin: 0; font-size: 16px;", 
          "✅ Welcome back! You've already rated before. Your ratings are saved.")
      )
    } else {
      intro_text <- div(
        style = "padding: 20px; background: linear-gradient(135deg, #2c7da015 0%, #014f8615 100%); border-radius: 12px; margin-bottom: 25px; text-align: center;",
        p(style = "color: #2d3748; font-weight: 600; margin: 0; font-size: 16px;",
          "🍩 Welcome! How would you think these Krapfen taste based on how they look and sound? Please rate each Krapfen on a scale of 1-10.")
      )
    }
    
    rating_divs <- lapply(krapfen, function(k) {
      # Use krapfen name directly as image filename with .png extension
      img_path <- paste0(k, ".png")
      default_value <- 5
      if(current_rater() %in% existing) {
        b <- background_data()
        # b has krapfen as a column and raters as rows
        default_value <- as.numeric(b[Rater == current_rater(), get(k)])
      }
      
      div(
        class = "rating-card",
        div(
          style = "display: flex; gap: 20px; align-items: flex-start;",
          div(
            style = "flex-shrink: 0;",
            img(
              src = img_path,
              width = "150px",
              height = "150px",
              style = "border-radius: 12px; object-fit: cover; box-shadow: 0 2px 8px rgba(0,0,0,0.1);"
            )
          ),
          div(
            style = "flex-grow: 1;",
            h4(k, style = "color: #2d3748; margin-top: 0;"),
            sliderInput(
              inputId = paste0("rating_", gsub(" ", "_", k)),
              label = "Rating (1-10):",
              min = 1,
              max = 10,
              value = default_value,
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
    if (!user_authenticated() & !is_new_user()) {
      app_mode("rate_single")
      return(NULL)
    }
    
    # Create button list based on whether it's a new user and if they've submitted
    buttons <- list(
      actionButton(
        inputId = "submit_ratings",
        label = "✓ Submit Ratings",
        class = "btn-primary",
        style = "padding: 12px 24px; font-size: 16px;"
      )
    )
    
    # Only show "View Results" button if it's an existing user OR new user has submitted
    if (!is_new_user() || new_user_submitted()) {
      buttons[[2]] <- actionButton(
        inputId = "view_results_btn",
        label = "📊 View Results",
        class = "btn-info",
        style = "padding: 12px 24px; font-size: 16px; margin-left: 10px;"
      )
    }
    
    # Add logout button
    buttons[[length(buttons) + 1]] <- actionButton(
      inputId = "cancel_ratings",
      label = "🚪 Logout",
      class = "btn-secondary",
      style = "padding: 12px 24px; font-size: 16px; margin-left: 10px;"
    )
    
    div(
      style = "padding: 20px;",
      div(
        class = "stats-card",
        style = "max-width: 900px; margin: 0 auto; padding: 30px;",
        h3(paste("🍩 Rate Krapfen as", current_rater()), style = "color: #2d3748; margin-top: 0; text-align: center;"),
        uiOutput("rating_inputs"),
        br(),
        div(
          style = "text-align: center;",
          do.call(tagList, buttons)
        ),
        div(id = "submit_message", style = "margin-top: 20px;")
      )
    )
  })
  
  output$single_rating_page <- renderUI({
    r <- copy(rating_data())
    if(current_rater() %in% r$Rater){
      krapfen_choices <- colnames(r)[which(is.na(r[Rater == current_rater()]))]
    }else{
      krapfen_choices <- krapfen_names()
    }
    
    div(
      style = "padding: 20px;",
      div(
        class = "stats-card",
        style = "max-width: 800px; margin: 0 auto; padding: 40px;",
        h3("Rate a Krapfen you've tried 🍩", style = "text-align: center; color: #2d3748; margin-top: 0;"),
        
        # Show existing ratings plot
        plotOutput("user_ratings_plot", height = "300px"),
        
        br(),
        
        pickerInput(
          "single_krapfen",
          "Select Krapfen:",
          choices = krapfen_choices,
          options = list(`live-search` = TRUE)
        ),
        
        sliderInput(
          "single_score",
          "Your rating:",
          min = 1, max = 10, value = 5, step = 1
        ),
        
        br(),
        
        div(
          style = "text-align: center;",
          actionButton(
            "submit_single_rating",
            "✓ Submit rating",
            class = "btn-primary",
            style = "padding: 12px 24px; font-size: 16px;"
          ),
          actionButton(
            "cancel_single_rating",
            "← Back",
            class = "btn-secondary",
            style = "padding: 12px 24px; font-size: 16px; margin-left: 10px;"
          )
        ),
        
        div(id = "single_submit_msg", style = "margin-top: 20px;")
      )
    )
  })
  
  # --- User ratings plot ---
  output$user_ratings_plot <- renderPlot({
    r <- copy(rating_data())
    user_row <- r[Rater == current_rater()]
    
    # Get only rated krapfen (non-NA values)
    user_ratings <- unlist(user_row[, -1])  # Exclude Rater column
    user_ratings <- user_ratings[!is.na(user_ratings)]
    
    if (length(user_ratings) == 0) {
      # If no ratings yet, show message
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "You haven't rated any Krapfen yet!", cex = 1.5, col = "gray40")
      return()
    }
    
    # Create data frame for plotting
    df <- data.frame(
      Krapfen = names(user_ratings),
      Rating = as.numeric(user_ratings)
    )
    
    ggplot(df, aes(x = reorder(Krapfen, Rating), y = Rating)) +
      geom_bar(stat = "identity", fill = "#3d98d3") +
      coord_flip() +
      ylim(0, 10) +
      labs(
        title = paste("Your Expert Ratings (", current_rater(), ")", sep = ""),
        x = "",
        y = "Rating"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  observeEvent(input$rate_single_btn, {
    app_mode("rate_single")
  })
  
  observeEvent(input$cancel_single_rating, {
    app_mode("view")
  })
  
  
  observeEvent(input$submit_single_rating, {
    req(input$single_krapfen, input$single_score)
    
    k <- input$single_krapfen
    score <- as.numeric(input$single_score)
    rater <- current_rater()
    
    df <- copy(rating_data())
    # convert all columns except Rater to numeric
    for (col in names(df)) {
      if (col != "Rater") {
        set(df, j = col, value = as.numeric(df[[col]]))
      }
    }
    
    if (!(rater %in% df$Rater)) {
      new_row <- as.list(rep(NA, ncol(df)))
      names(new_row) <- names(df)
      new_row$Rater <- rater
      df <- rbind(df, as.data.table(new_row), fill = TRUE)
    }
    
    df[Rater == rater, (k) := score]
    
    fwrite(df, file.path(data_dir, "real_ratings.csv"))
    
    rating_data(df) 
    app_mode("view")
    
    shinyjs::runjs(
      "document.getElementById('single_submit_msg').innerHTML =
     '<div class=\"alert alert-success\">Rating saved 🎉</div>';"
    )
  })
  
  
  
  # --- Submit ratings handler -----------------------------------------------
  observeEvent(input$submit_ratings, {

    rater_name <- current_rater()    
    current_data <- background_data()    
    actual_krapfen <- krapfen_names()
    csv_file = file.path(data_dir, "KrapfenRating.csv")
    
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
    background_data(current_data)

    # Update existing users (if new user)
    if (!(rater_name %in% existing_raters())) {
      existing <- c(existing_raters(), rater_name)
      existing_raters(existing)
    }
    
    # Mark submission and user as no longer new (for existing users who re-rate)
    new_user_submitted(TRUE)
    is_new_user(FALSE)
    app_mode("view")
  })
  
  # --- Logout handler -------------------------------------------------------
  observeEvent(input$logout_btn, {
    user_authenticated(FALSE)
    is_new_user(FALSE)
    new_user_submitted(FALSE)
    current_rater("")
    updateTextInput(session, "rater_name", value = "")
    app_mode("login_page_ui")
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
    app_mode("rate_all")
  })
  
  # --- Correlation plot -----------------------------------------------------
  output$corrPlot <- renderPlot({
    b <- copy(background_data())
    bt <- transpose_background(b)
    
    # Check if we have enough raters for correlation
    if (nrow(bt) < 2) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Need at least 2 raters to compute correlations", 
           cex = 1.5, col = "gray40")
      return()
    }
    
    corr_method <- ifelse(input$corr_method_switch, "spearman", "pearson")
    
    corr_matrix <- cor(
      bt,
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
    b <- copy(background_data())
    bt <- transpose_background(b)

    bt[, Mean_Rating := rowSums(.SD) / length(colnames(bt))]
    bt[, Krapfen := colnames(b)]
    # Sort by rating
    bt <- bt[order(-Mean_Rating)]
    bt[, Krapfen := factor(Krapfen, levels = Krapfen)]

    # compute standard deviation of Summed Rating
    bt[, SD_Rating := apply(.SD, 1, sd, na.rm = TRUE), .SDcols= colnames(bt)]
    
    # draw images instead of scatter points
    bt[, image := paste0("www/", Krapfen, ".png")]
    image_grobs <- lapply(bt$image, function(path) {
      rasterGrob(readPNG(path), interpolate = TRUE)
    })
    names(image_grobs) <- bt$Krapfen
    
    p <- ggplot(bt, aes(x = Krapfen, y = Mean_Rating)) +
      geom_errorbar(aes(ymin = Mean_Rating - SD_Rating,
                        ymax = Mean_Rating + SD_Rating),
                    width = 0.2) +
      theme_minimal() +
      ylim(1, 10) +
      # add y axis ticks at every integer
      scale_y_continuous(breaks = seq(1, 10, by = 1)) +
      labs(
        title = "Average Krapfen Ratings",
        x = "Krapfen",
        y = "Average Score"
      ) +
      theme(axis.text.x = element_blank(),
            text = element_text(size = 16))
    
    for (i in seq_len(nrow(bt))) {
      k <- bt$Krapfen[i]
      p <- p + annotation_custom(
        grob = image_grobs[[as.character(k)]],
        xmin = i - 0.5,
        xmax = i + 0.5,
        ymin = bt$Mean_Rating[i] - 0.5,
        ymax = bt$Mean_Rating[i] + 0.5
      )
    }
    
    p
    
  })
  
  # --- Render posterior plot ---
  output$posteriorPlot <- renderPlot({
    req(input$selected_krapfen)
    
    krapfen <- input$selected_krapfen
    
    # survey data for selected krapfen
    background_vec <- copy(background_data())[[krapfen]]
    
    # background data
    rating_vec <- as.numeric(copy(rating_data())[[krapfen]])
    
    plot_comparison(survey_data = background_vec, 
                    expert_opinion = rating_vec)
  })
}

shinyApp(ui, server)
