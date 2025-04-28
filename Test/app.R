# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(viridis)
library(janitor)
library(forcats)
library(plotly)         # For interactive plots
library(DT)             # For interactive tables
library(corrplot)       # For correlation analysis
library(cluster)        # For clustering analysis
library(factoextra)     # For cluster visualization
library(ggridges)       # For density ridge plots
library(bslib)          # For better styling
library(shinydashboard) # For professional dashboard layout

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(
    title = "Coffee Consumer Insights",
    # Add notification dropdown for data status
    dropdownMenu(type = "notifications",
                 icon = icon("info-circle"),
                 badgeStatus = "info",
                 headerText = "Dashboard Info",
                 notificationItem(
                   text = "Current Status",
                   icon = icon("info-circle"),
                   status = "info"
                 )
    )
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Consumption Habits", tabName = "consumption", icon = icon("coffee")),
      menuItem("Flavor Preferences", tabName = "preferences", icon = icon("thumbs-up")),
      menuItem("Coffee Comparisons", tabName = "comparisons", icon = icon("chart-bar")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Advanced Analytics", tabName = "advanced", icon = icon("chart-line")),
      menuItem("Consumer Segments", tabName = "segments", icon = icon("user-tag")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      
      br(),
      
      # Filter panel
      div(
        style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; margin: 0 15px;",
        h4("Data Filters", style = "margin-top: 0; color: #000000;"),
        
        # Reset filters button
        actionButton("resetFilters", "Reset All Filters",
                     class = "btn-sm btn-primary", width = "100%"),
        
        br(), br(),
        
        # Age filter
        selectInput("selected_age", "Age Group:",
                    choices = c("All" = "All"),
                    selected = "All"),
        
        # Gender filter
        selectInput("selected_gender", "Gender:",
                    choices = c("All" = "All"),
                    selected = "All"),
        
        # Roast level filter
        selectInput("selected_roast", "Roast Level:",
                    choices = c("All" = "All"),
                    selected = "All"),
        
        hr(),
        
        # Coffee comparison selection
        selectInput("coffee_comparison", "Coffee Comparison:",
                    choices = c(
                      "Coffee A vs B" = "ab",
                      "Coffee A vs C" = "ac",
                      "Coffee B vs C" = "bc",
                      "Coffee A vs D" = "ad"
                    ),
                    selected = "ab"),
        
        # Download button
        downloadButton("downloadData", "Download Data", class = "btn-sm btn-primary")
      )
    )
  ),
  
  # Dashboard body
  dashboardBody(
    # Include custom CSS with improved text color styling
    tags$head(
      tags$style(HTML("
        .small-box {height: 120px;}
        .small-box .icon-large {top: 5px;}
        .content-wrapper, .right-side {background-color: #f8f9fa;}
        .box {border-top: 3px solid #3c8dbc;}
        
        /* Text color fixes for filter section */
        .sidebar .selectInput label, .sidebar .control-label {
          color: #000000 !important;
          font-weight: 600;
        }
        .sidebar select, .sidebar .selectize-input {
          color: #000000 !important;
        }
        .sidebar h4 {
          color: #000000 !important;
          font-weight: 700;
        }
        .sidebar hr {
          border-top: 1px solid rgba(0, 0, 0, 0.2);
        }
        .sidebar .dropdown-toggle {
          color: #000000 !important;
        }
        /* Make filter panel background slightly darker for better contrast */
        .sidebar .shiny-input-container {
          background-color: #ffffff;
          border-radius: 4px;
          padding: 2px;
        }
      "))
    ),
    
    # Show data status for debugging
    fluidRow(
      column(12,
             div(
               style = "display: none;", # Hide by default, can be shown for debugging
               verbatimTextOutput("dataStatus"),
               verbatimTextOutput("filteredCount")
             )
      )
    ),
    
    # Main content
    tabItems(
      # Dashboard Overview tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("totalParticipantsBox", width = 3),
                valueBoxOutput("avgExpertiseBox", width = 3),
                valueBoxOutput("topRoastBox", width = 3),
                valueBoxOutput("topCoffeeBox", width = 3)
              ),
              
              fluidRow(
                box(title = "Coffee Consumption By Age", status = "primary", solidHeader = TRUE,
                    plotlyOutput("dashboardAge", height = 300), width = 6),
                box(title = "Coffee Preference Distribution", status = "primary", solidHeader = TRUE,
                    plotlyOutput("dashboardPreferences", height = 300), width = 6)
              ),
              
              fluidRow(
                box(title = "Key Insights", status = "info", solidHeader = TRUE, width = 12,
                    htmlOutput("keyInsights"))
              )
      ),
      
      # Consumption Habits tab
      tabItem(tabName = "consumption",
              fluidRow(
                box(title = "Daily Coffee Consumption", status = "primary", solidHeader = TRUE,
                    plotlyOutput("cupsPlotly"), width = 6),
                box(title = "Where Coffee is Consumed", status = "primary", solidHeader = TRUE,
                    plotlyOutput("wherePlotly"), width = 6)
              ),
              
              fluidRow(
                box(title = "Monthly Coffee Expenditure", status = "primary", solidHeader = TRUE,
                    plotlyOutput("spendPlotly"), width = 12)
              ),
              
              fluidRow(
                box(title = "Coffee Consumption Frequency by Location", status = "primary", solidHeader = TRUE,
                    plotlyOutput("consumptionHeatmap"), width = 12)
              )
      ),
      
      # Flavor Preferences tab
      tabItem(tabName = "preferences",
              fluidRow(
                box(title = "Preferred Roast Level", status = "primary", solidHeader = TRUE,
                    plotlyOutput("roastPlotly"), width = 6),
                box(title = "Preferred Coffee Strength", status = "primary", solidHeader = TRUE,
                    plotlyOutput("strengthPlotly"), width = 6)
              ),
              
              fluidRow(
                box(title = "Top Favorite Coffee Drinks", status = "primary", solidHeader = TRUE,
                    plotlyOutput("favoritePlotly"), width = 12)
              ),
              
              fluidRow(
                box(title = "Flavor Preference by Age Group", status = "primary", solidHeader = TRUE,
                    plotlyOutput("flavorByAge"), width = 12)
              )
      ),
      
      # Coffee Comparisons tab
      tabItem(tabName = "comparisons",
              fluidRow(
                box(title = "Coffee Rating Comparison", status = "primary", solidHeader = TRUE,
                    plotlyOutput("comparisonPlotly", height = 500), width = 12)
              ),
              
              fluidRow(
                box(title = "Overall Coffee Preference Distribution", status = "primary", solidHeader = TRUE,
                    plotOutput("overallPreference"), width = 6),
                box(title = "Rating Correlation Matrix", status = "primary", solidHeader = TRUE,
                    plotOutput("ratingCorrelation"), width = 6)
              )
      ),
      
      # Demographics tab
      tabItem(tabName = "demographics",
              fluidRow(
                box(title = "Age Distribution", status = "primary", solidHeader = TRUE,
                    plotlyOutput("agePlotly"), width = 6),
                box(title = "Gender Distribution", status = "primary", solidHeader = TRUE,
                    plotlyOutput("genderPlotly"), width = 6)
              ),
              
              fluidRow(
                box(title = "Education Level Distribution", status = "primary", solidHeader = TRUE,
                    plotlyOutput("educationPlotly"), width = 12)
              ),
              
              fluidRow(
                box(title = "Demographics vs. Coffee Preference", status = "primary", solidHeader = TRUE,
                    plotOutput("demographicPreferences"), width = 12)
              )
      ),
      
      # Advanced Analytics tab
      tabItem(tabName = "advanced",
              fluidRow(
                box(title = "Rating Distribution Across All Coffees", status = "primary", solidHeader = TRUE,
                    plotOutput("ratingDistribution", height = 300), width = 12)
              ),
              
              fluidRow(
                tabBox(
                  title = "Advanced Analysis",
                  width = 12,
                  tabPanel("Correlation Analysis",
                           plotOutput("correlationPlot", height = 500),
                           br(),
                           htmlOutput("correlationInsights")),
                  tabPanel("Principal Component Analysis",
                           plotOutput("pcaPlot", height = 500),
                           br(),
                           htmlOutput("pcaInsights")),
                  tabPanel("Bitterness vs. Preference",
                           plotOutput("bitterPreference", height = 500),
                           br(),
                           htmlOutput("bitterInsights"))
                )
              )
      ),
      
      # Consumer Segments tab
      tabItem(tabName = "segments",
              fluidRow(
                box(width = 3,
                    selectInput("numClusters", "Number of Clusters:",
                                choices = c(2, 3, 4, 5, 6),
                                selected = 3),
                    selectInput("clusterVars", "Clustering Variables:",
                                choices = c("Demographics" = "demo",
                                            "Preferences" = "pref",
                                            "Both" = "both"),
                                selected = "both"),
                    actionButton("runClustering", "Run Clustering Analysis", class = "btn-primary"),
                    hr(),
                    htmlOutput("clusterSummary")
                ),
                box(title = "Consumer Segments", status = "primary", solidHeader = TRUE,
                    plotOutput("segmentPlot", height = 500), width = 9)
              ),
              
              fluidRow(
                box(title = "Segment Profiles", status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("segmentProfiles"), width = 12)
              )
      ),
      
      # Data Explorer tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(title = "Data Explorer", status = "primary", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("dataTable"))
              ),
              
              fluidRow(
                box(title = "Column Information", status = "primary", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("columnInfo"))
              )
      ),
      
      # About tab
      tabItem(tabName = "about",
              fluidRow(
                box(title = "About this Dashboard", status = "primary", solidHeader = TRUE, width = 12,
                    HTML("
                      <h4>Coffee Consumer Preferences Dashboard</h4>
                      <p>This interactive dashboard explores data from the 'Great American Coffee Taste Test' conducted by James Hoffmann and Cometeer in October 2023.</p>
                      <h4>Data Source</h4>
                      <p>Participants tasted four different coffees (A-D) and rated their bitterness, acidity, and personal preference, along with providing information about their coffee consumption habits and demographic details.</p>
                      <h4>Features</h4>
                      <ul>
                        <li><strong>Basic Analysis:</strong> Explore coffee consumption habits, preferences, and demographic breakdowns</li>
                        <li><strong>Advanced Analytics:</strong> Correlation analysis, PCA, and clustering to identify patterns</li>
                        <li><strong>Consumer Segmentation:</strong> Identify and profile different types of coffee consumers</li>
                        <li><strong>Interactive Exploration:</strong> Filter data, compare coffees, and download results</li>
                      </ul>
                    ")
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Data status tracking
  data_status <- reactiveVal("Initializing...")
  
  # Load and clean data function
  raw_data <- reactive({
    data_status("Loading data...")
    
    # Try loading from local file first
    local_file <- "coffee_survey_raw.csv"
    
    if (file.exists(local_file)) {
      data_status("Loading data from local file...")
      tryCatch({
        data <- read_csv(local_file, show_col_types = FALSE)
        data_status(paste("Data loaded from local file. Rows:", nrow(data), "Columns:", ncol(data)))
        return(data)
      }, error = function(e) {
        data_status(paste("Error loading local file:", e$message))
      })
    }
    
    # Download from GitHub if local file doesn't exist or couldn't be loaded
    data_status("Downloading data from GitHub...")
    tryCatch({
      url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-14/coffee_survey.csv'
      data <- read_csv(url, show_col_types = FALSE)
      
      # Save for future use
      write_csv(data, local_file)
      
      data_status(paste("Data downloaded from GitHub. Rows:", nrow(data), "Columns:", ncol(data)))
      return(data)
    }, error = function(e) {
      data_status(paste("Error downloading data:", e$message))
      return(data.frame()) # Return empty data frame on error
    })
  })
  
  # Clean and process data
  processed_data <- reactive({
    data <- raw_data()
    
    if (nrow(data) == 0) {
      data_status("No data available to process")
      return(data.frame())
    }
    
    data_status("Processing and cleaning data...")
    
    # Clean the data
    tryCatch({
      clean_data <- data %>%
        # Clean column names
        clean_names() %>%
        # Convert numeric ratings
        mutate(across(matches("coffee_[a-d]_(bitterness|acidity|personal_preference)"), as.numeric)) %>%
        # Handle NA values
        mutate(
          across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)),
          across(where(is.character), ~ifelse(is.na(.), "Unknown", .))
        )
      
      # Create composite scores for each coffee
      clean_data <- clean_data %>%
        mutate(
          coffee_a_score = (coffee_a_personal_preference * 2 - coffee_a_bitterness * 0.5 - coffee_a_acidity * 0.5) / 3,
          coffee_b_score = (coffee_b_personal_preference * 2 - coffee_b_bitterness * 0.5 - coffee_b_acidity * 0.5) / 3,
          coffee_c_score = (coffee_c_personal_preference * 2 - coffee_c_bitterness * 0.5 - coffee_c_acidity * 0.5) / 3,
          coffee_d_score = (coffee_d_personal_preference * 2 - coffee_d_bitterness * 0.5 - coffee_d_acidity * 0.5) / 3
        )
      
      # Derive variables for analytics
      clean_data <- clean_data %>%
        mutate(
          favorite_coffee = case_when(
            coffee_a_personal_preference >= coffee_b_personal_preference &
              coffee_a_personal_preference >= coffee_c_personal_preference &
              coffee_a_personal_preference >= coffee_d_personal_preference ~ "Coffee A",
            coffee_b_personal_preference >= coffee_a_personal_preference &
              coffee_b_personal_preference >= coffee_c_personal_preference &
              coffee_b_personal_preference >= coffee_d_personal_preference ~ "Coffee B",
            coffee_c_personal_preference >= coffee_a_personal_preference &
              coffee_c_personal_preference >= coffee_b_personal_preference &
              coffee_c_personal_preference >= coffee_d_personal_preference ~ "Coffee C",
            coffee_d_personal_preference >= coffee_a_personal_preference &
              coffee_d_personal_preference >= coffee_b_personal_preference &
              coffee_d_personal_preference >= coffee_c_personal_preference ~ "Coffee D",
            TRUE ~ "Unknown"
          ),
          avg_pref_score = (coffee_a_personal_preference + coffee_b_personal_preference +
                              coffee_c_personal_preference + coffee_d_personal_preference) / 4,
          avg_bitterness = (coffee_a_bitterness + coffee_b_bitterness +
                              coffee_c_bitterness + coffee_d_bitterness) / 4,
          avg_acidity = (coffee_a_acidity + coffee_b_acidity +
                           coffee_c_acidity + coffee_d_acidity) / 4,
          bitterness_sensitivity = case_when(
            avg_bitterness < 3 ~ "Low",
            avg_bitterness < 7 ~ "Medium",
            TRUE ~ "High"
          ),
          acidity_sensitivity = case_when(
            avg_acidity < 3 ~ "Low",
            avg_acidity < 7 ~ "Medium",
            TRUE ~ "High"
          )
        )
      
      data_status("Data processing complete")
      return(clean_data)
      
    }, error = function(e) {
      data_status(paste("Error processing data:", e$message))
      return(data)
    })
  })
  
  # Update UI with filter options
  observe({
    df <- processed_data()
    
    if (nrow(df) == 0) return()
    
    # Update age filter
    if ("age" %in% names(df)) {
      age_values <- unique(df$age)
      age_values <- age_values[!is.na(age_values)]
      if (length(age_values) > 0) {
        updateSelectInput(session, "selected_age",
                          choices = c("All" = "All", sort(age_values)))
      }
    }
    
    # Update gender filter
    if ("gender" %in% names(df)) {
      gender_values <- unique(df$gender)
      gender_values <- gender_values[!is.na(gender_values)]
      if (length(gender_values) > 0) {
        updateSelectInput(session, "selected_gender",
                          choices = c("All" = "All", sort(gender_values)))
      }
    }
    
    # Update roast level filter
    if ("roast_level" %in% names(df)) {
      roast_values <- unique(df$roast_level)
      roast_values <- roast_values[!is.na(roast_values)]
      if (length(roast_values) > 0) {
        updateSelectInput(session, "selected_roast",
                          choices = c("All" = "All", sort(roast_values)))
      }
    }
  })
  
  # Reset filters button
  observeEvent(input$resetFilters, {
    updateSelectInput(session, "selected_age", selected = "All")
    updateSelectInput(session, "selected_gender", selected = "All")
    updateSelectInput(session, "selected_roast", selected = "All")
  })
  
  # Filter data based on selections
  filtered_data <- reactive({
    df <- processed_data()
    
    if (nrow(df) == 0) return(data.frame())
    
    # Apply filters
    if (input$selected_age != "All" && "age" %in% names(df)) {
      df <- df %>% filter(age == input$selected_age)
    }
    
    if (input$selected_gender != "All" && "gender" %in% names(df)) {
      df <- df %>% filter(gender == input$selected_gender)
    }
    
    if (input$selected_roast != "All" && "roast_level" %in% names(df)) {
      df <- df %>% filter(roast_level == input$selected_roast)
    }
    
    return(df)
  })
  
  # Create filter description for plot titles
  filter_desc <- reactive({
    filters <- c()
    
    if (input$selected_age != "All") filters <- c(filters, paste("Age:", input$selected_age))
    if (input$selected_gender != "All") filters <- c(filters, paste("Gender:", input$selected_gender))
    if (input$selected_roast != "All") filters <- c(filters, paste("Roast:", input$selected_roast))
    
    if (length(filters) == 0) return("All Data")
    return(paste(filters, collapse = ", "))
  })
  
  # Display data status
  output$dataStatus <- renderText({
    data_status()
  })
  
  # Display filtered record count
  output$filteredCount <- renderText({
    df <- filtered_data()
    paste("Records after filtering:", nrow(df))
  })
  
  # Value Boxes for Dashboard
  output$totalParticipantsBox <- renderValueBox({
    df <- filtered_data()
    valueBox(
      nrow(df), "Total Participants",
      icon = icon("users"), color = "blue"
    )
  })
  
  output$avgExpertiseBox <- renderValueBox({
    df <- filtered_data()
    if (nrow(df) == 0 || !"expertise" %in% names(df)) {
      avg_exp <- "N/A"
    } else {
      avg_exp <- round(mean(df$expertise, na.rm = TRUE), 1)
    }
    valueBox(
      avg_exp, "Avg. Expertise (1-10)",
      icon = icon("star"), color = "yellow"
    )
  })
  
  output$topRoastBox <- renderValueBox({
    df <- filtered_data()
    if (nrow(df) == 0 || !"roast_level" %in% names(df)) {
      top_roast <- "N/A"
    } else {
      top_roast <- df %>%
        count(roast_level) %>%
        arrange(desc(n)) %>%
        slice(1) %>%
        pull(roast_level)
    }
    valueBox(
      top_roast, "Most Popular Roast",
      icon = icon("fire"), color = "red"
    )
  })
  
  output$topCoffeeBox <- renderValueBox({
    df <- filtered_data()
    if (nrow(df) == 0 || !"favorite_coffee" %in% names(df)) {
      top_coffee <- "N/A"
    } else {
      top_coffee <- df %>%
        count(favorite_coffee) %>%
        arrange(desc(n)) %>%
        slice(1) %>%
        pull(favorite_coffee)
    }
    valueBox(
      top_coffee, "Most Preferred Coffee",
      icon = icon("coffee"), color = "green"
    )
  })
  
  # Key Insights Output
  output$keyInsights <- renderUI({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(HTML("<p>No data available for insights</p>"))
    }
    
    # Generate insights based on the data
    insights <- "<h4>Key Findings:</h4><ul>"
    
    # Insight 1: Average expertise level
    if ("expertise" %in% names(df)) {
      avg_exp <- round(mean(df$expertise, na.rm = TRUE), 1)
      insights <- paste0(insights,
                         "<li>The average self-reported coffee expertise level is <b>",
                         avg_exp, " out of 10</b>, indicating that participants are ",
                         ifelse(avg_exp > 7, "very knowledgeable",
                                ifelse(avg_exp > 5, "moderately knowledgeable", "relatively new")),
                         " coffee drinkers.</li>")
    }
    
    # Insight 2: Most common place to drink
    if ("where_drink" %in% names(df)) {
      top_place <- df %>%
        count(where_drink) %>%
        arrange(desc(n)) %>%
        slice(1)
      
      if (nrow(top_place) > 0) {
        place_percent <- round(top_place$n / nrow(df) * 100, 1)
        insights <- paste0(insights,
                           "<li><b>", place_percent, "%</b> of participants most commonly drink coffee at <b>",
                           top_place$where_drink, "</b>.</li>")
      }
    }
    
    # Insight 3: Preference patterns
    if (all(c("coffee_a_personal_preference", "coffee_b_personal_preference",
              "coffee_c_personal_preference", "coffee_d_personal_preference") %in% names(df))) {
      avg_a <- mean(df$coffee_a_personal_preference, na.rm = TRUE)
      avg_b <- mean(df$coffee_b_personal_preference, na.rm = TRUE)
      avg_c <- mean(df$coffee_c_personal_preference, na.rm = TRUE)
      avg_d <- mean(df$coffee_d_personal_preference, na.rm = TRUE)
      
      best_coffee <- which.max(c(avg_a, avg_b, avg_c, avg_d))
      best_coffee_letter <- c("A", "B", "C", "D")[best_coffee]
      best_coffee_score <- round(c(avg_a, avg_b, avg_c, avg_d)[best_coffee], 1)
      
      insights <- paste0(insights,
                         "<li>Coffee <b>", best_coffee_letter,
                         "</b> had the highest average preference rating at <b>",
                         best_coffee_score, " out of 10</b>.</li>")
    }
    
    # Insight 4: Consumption patterns
    if ("cups" %in% names(df)) {
      top_cups <- df %>%
        count(cups) %>%
        arrange(desc(n)) %>%
        slice(1)
      
      if (nrow(top_cups) > 0) {
        cups_percent <- round(top_cups$n / nrow(df) * 100, 1)
        insights <- paste0(insights,
                           "<li>The most common consumption is <b>", top_cups$cups,
                           " cups per day</b>, reported by <b>", cups_percent, "%</b> of participants.</li>")
      }
    }
    
    insights <- paste0(insights, "</ul>")
    HTML(insights)
  })
  
  # Data table for explorer tab
  output$dataTable <- DT::renderDataTable({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(data.frame(message = "No data available"))
    }
    
    DT::datatable(df,
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    searchHighlight = TRUE
                  ),
                  filter = 'top',
                  rownames = FALSE)
  })
  
  # Column information for explorer tab
  output$columnInfo <- DT::renderDataTable({
    df <- processed_data()
    
    if (ncol(df) == 0) {
      return(data.frame(message = "No data available"))
    }
    
    col_info <- data.frame(
      Column = names(df),
      Type = sapply(df, class),
      UniqueValues = sapply(df, function(x) length(unique(x))),
      PercentMissing = sapply(df, function(x) round(sum(is.na(x)) / length(x) * 100, 2))
    )
    
    DT::datatable(col_info,
                  options = list(
                    pageLength = 10,
                    searchHighlight = TRUE
                  ),
                  filter = 'top',
                  rownames = FALSE)
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("coffee-data-filtered-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # INTERACTIVE VISUALIZATIONS (PLOTLY)
  
  # Dashboard Age Chart
  output$dashboardAge <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"age" %in% names(df)) {
      return(NULL)
    }
    
    # Prepare data
    plot_data <- df %>%
      count(age) %>%
      arrange(desc(n))
    
    # Create plot
    p <- plot_ly(plot_data, x = ~age, y = ~n, type = "bar",
                 marker = list(color = "#3c8dbc")) %>%
      layout(title = "Age Distribution",
             xaxis = list(title = "Age Group"),
             yaxis = list(title = "Count"))
    
    return(p)
  })
  
  # Dashboard Preferences Chart
  output$dashboardPreferences <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"favorite_coffee" %in% names(df)) {
      return(NULL)
    }
    
    # Prepare data
    plot_data <- df %>%
      count(favorite_coffee) %>%
      arrange(desc(n))
    
    # Create plot
    p <- plot_ly(plot_data, labels = ~favorite_coffee, values = ~n, type = "pie",
                 marker = list(colors = viridis::viridis(nrow(plot_data)))) %>%
      layout(title = "Favorite Coffee Distribution")
    
    return(p)
  })
  
  # Daily cups (plotly)
  output$cupsPlotly <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"cups" %in% names(df)) {
      return(NULL)
    }
    
    # Prepare data
    plot_data <- df %>%
      count(cups) %>%
      arrange(desc(n))
    
    # Create plot
    p <- plot_ly(plot_data, x = ~cups, y = ~n, type = "bar",
                 marker = list(color = "#3c8dbc")) %>%
      layout(title = paste("Daily Coffee Consumption -", filter_desc()),
             xaxis = list(title = "Cups per Day"),
             yaxis = list(title = "Count"))
    
    return(p)
  })
  
  # Where coffee is consumed (plotly)
  output$wherePlotly <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"where_drink" %in% names(df)) {
      return(NULL)
    }
    
    # Prepare data
    plot_data <- df %>%
      count(where_drink) %>%
      arrange(desc(n)) %>%
      head(10)
    
    # Create plot
    p <- plot_ly(plot_data, x = ~reorder(where_drink, -n), y = ~n, type = "bar",
                 marker = list(color = "#00a65a")) %>%
      layout(title = paste("Where Coffee is Consumed -", filter_desc()),
             xaxis = list(title = "Location", categoryorder = "array", categoryarray = ~reorder(where_drink, -n)),
             yaxis = list(title = "Count"))
    
    return(p)
  })
  
  # Monthly spending (plotly)
  output$spendPlotly <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"total_spend" %in% names(df)) {
      return(NULL)
    }
    
    # Spending order
    spend_order <- c("$0-$20", "$21-$40", "$41-$60", "$61-$80", "$81-$100",
                     "$101-$150", "$150-$200", ">$200")
    
    # Check if the values match our expected levels
    actual_values <- unique(df$total_spend)
    known_values <- intersect(spend_order, actual_values)
    
    if (length(known_values) > 0) {
      # Create ordered factor
      df <- df %>%
        mutate(total_spend = factor(total_spend,
                                    levels = c(known_values, setdiff(actual_values, known_values))))
    }
    
    # Prepare data
    plot_data <- df %>%
      count(total_spend) %>%
      arrange(desc(n))
    
    # Create plot
    p <- plot_ly(plot_data, x = ~total_spend, y = ~n, type = "bar",
                 marker = list(color = "#f39c12")) %>%
      layout(title = paste("Monthly Coffee Expenditure -", filter_desc()),
             xaxis = list(title = "Monthly Spending"),
             yaxis = list(title = "Count"))
    
    return(p)
  })
  
  # Consumption by location heatmap
  output$consumptionHeatmap <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"where_drink" %in% names(df) || !"cups" %in% names(df)) {
      return(NULL)
    }
    
    # Prepare data - cross-tabulate location and cups per day
    plot_data <- df %>%
      group_by(where_drink, cups) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    # Limit to top 10 locations and 5 cup counts for readability
    top_locations <- df %>% count(where_drink) %>% arrange(desc(n)) %>% head(10) %>% pull(where_drink)
    plot_data <- plot_data %>%
      filter(where_drink %in% top_locations) %>%
      arrange(desc(count))
    
    # Create plot
    p <- plot_ly(plot_data, x = ~cups, y = ~where_drink, z = ~count, type = "heatmap",
                 colorscale = "Viridis") %>%
      layout(title = "Coffee Consumption by Location",
             xaxis = list(title = "Cups per Day"),
             yaxis = list(title = "Location", categoryorder = "array",
                          categoryarray = rev(top_locations)))
    
    return(p)
  })
  
  # Roast level (plotly)
  output$roastPlotly <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"roast_level" %in% names(df)) {
      return(NULL)
    }
    
    # Prepare data
    plot_data <- df %>%
      count(roast_level) %>%
      arrange(desc(n))
    
    # Define custom colors for roast levels
    roast_colors <- c("Light" = "#E3C878", "Medium" = "#B56727", "Dark" = "#6F4E37", "Unknown" = "#CCCCCC")
    colors <- roast_colors[plot_data$roast_level]
    
    # Create plot
    p <- plot_ly(plot_data, x = ~reorder(roast_level, -n), y = ~n, type = "bar",
                 marker = list(color = colors)) %>%
      layout(title = paste("Preferred Roast Level -", filter_desc()),
             xaxis = list(title = "Roast Level"),
             yaxis = list(title = "Count"))
    
    return(p)
  })
  
  # Coffee strength (plotly)
  output$strengthPlotly <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"strength" %in% names(df)) {
      return(NULL)
    }
    
    # Strength order
    strength_order <- c("Weak", "Somewhat weak", "Medium", "Somewhat strong", "Strong")
    
    # Prepare data
    plot_data <- df %>%
      count(strength) %>%
      arrange(desc(n))
    
    # Create plot
    p <- plot_ly(plot_data, x = ~reorder(strength, -n), y = ~n, type = "bar",
                 marker = list(color = colorRamp(c("#E3C878", "#6F4E37"))(seq(0, 1, length.out = nrow(plot_data))))) %>%
      layout(title = paste("Preferred Coffee Strength -", filter_desc()),
             xaxis = list(title = "Strength"),
             yaxis = list(title = "Count"))
    
    return(p)
  })
  
  # Favorite coffee drinks (plotly)
  output$favoritePlotly <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"favorite" %in% names(df)) {
      return(NULL)
    }
    
    # Prepare data - top 10 favorite drinks
    plot_data <- df %>%
      count(favorite) %>%
      arrange(desc(n)) %>%
      head(10)
    
    # Create plot
    p <- plot_ly(plot_data, y = ~reorder(favorite, n), x = ~n, type = "bar", orientation = 'h',
                 marker = list(color = "#00c0ef")) %>%
      layout(title = paste("Top 10 Favorite Coffee Drinks -", filter_desc()),
             xaxis = list(title = "Count"),
             yaxis = list(title = ""))
    
    return(p)
  })
  
  # Flavor by age (plotly)
  output$flavorByAge <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"age" %in% names(df) ||
        !"bitterness_sensitivity" %in% names(df) ||
        !"acidity_sensitivity" %in% names(df)) {
      return(NULL)
    }
    
    # Prepare data - cross-tabulate age and flavor sensitivity
    bitterness_data <- df %>%
      group_by(age, bitterness_sensitivity) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(sensitivity_type = "Bitterness")
    
    acidity_data <- df %>%
      group_by(age, acidity_sensitivity) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(sensitivity_type = "Acidity")
    
    plot_data <- bind_rows(
      bitterness_data %>% rename(sensitivity = bitterness_sensitivity),
      acidity_data %>% rename(sensitivity = acidity_sensitivity)
    )
    
    # Create plot
    p <- plot_ly(plot_data, x = ~age, y = ~count, color = ~sensitivity, type = "bar",
                 facet = ~sensitivity_type) %>%
      layout(title = "Flavor Sensitivity by Age Group",
             xaxis = list(title = "Age Group"),
             yaxis = list(title = "Count"),
             barmode = "stack")
    
    return(p)
  })
  
  # Coffee comparison (plotly) - MODIFIED VERSION TO FIX THE ERROR
  output$comparisonPlotly <- renderPlotly({
    df <- filtered_data()
    
    # Function to check if columns exist
    coffee_columns_exist <- function(coffee_letter) {
      prefix <- paste0("coffee_", coffee_letter, "_")
      bitter_col <- paste0(prefix, "bitterness")
      acid_col <- paste0(prefix, "acidity")
      pref_col <- paste0(prefix, "personal_preference")
      
      all(c(bitter_col, acid_col, pref_col) %in% names(df))
    }
    
    # Get coffee letters from comparison choice
    coffee_pair <- strsplit(input$coffee_comparison, "")[[1]]
    coffee1 <- coffee_pair[1]
    coffee2 <- coffee_pair[2]
    
    # Check if we have data and required columns
    if (nrow(df) == 0 || !coffee_columns_exist(coffee1) || !coffee_columns_exist(coffee2)) {
      return(NULL)
    }
    
    # Prepare the data for comparison
    comparison_data <- df %>%
      select(
        coffee1_bitter = paste0("coffee_", coffee1, "_bitterness"),
        coffee1_acid = paste0("coffee_", coffee1, "_acidity"),
        coffee1_pref = paste0("coffee_", coffee1, "_personal_preference"),
        coffee2_bitter = paste0("coffee_", coffee2, "_bitterness"),
        coffee2_acid = paste0("coffee_", coffee2, "_acidity"),
        coffee2_pref = paste0("coffee_", coffee2, "_personal_preference")
      ) %>%
      pivot_longer(
        cols = everything(),
        names_to = c("coffee", "attribute"),
        names_pattern = "coffee([12])_(.*)",
        values_to = "rating"
      ) %>%
      mutate(
        coffee = case_when(
          coffee == "1" ~ paste("Coffee", toupper(coffee1)),
          coffee == "2" ~ paste("Coffee", toupper(coffee2))
        ),
        attribute = case_when(
          attribute == "bitter" ~ "Bitterness",
          attribute == "acid" ~ "Acidity",
          attribute == "pref" ~ "Personal Preference"
        )
      )
    
    # Split the data by attribute
    bitterness_data <- comparison_data %>% filter(attribute == "Bitterness")
    acidity_data <- comparison_data %>% filter(attribute == "Acidity")
    preference_data <- comparison_data %>% filter(attribute == "Personal Preference")
    
    # Create separate plots for each attribute
    p1 <- plot_ly(bitterness_data, y = ~rating, color = ~coffee, type = "box") %>%
      layout(title = "Bitterness",
             yaxis = list(title = "Rating"),
             showlegend = FALSE)
    
    p2 <- plot_ly(acidity_data, y = ~rating, color = ~coffee, type = "box") %>%
      layout(title = "Acidity",
             yaxis = list(title = "Rating"),
             showlegend = FALSE)
    
    p3 <- plot_ly(preference_data, y = ~rating, color = ~coffee, type = "box") %>%
      layout(title = "Personal Preference",
             yaxis = list(title = "Rating"),
             showlegend = TRUE)
    
    # Combine the plots using subplot
    p <- subplot(p1, p2, p3, nrows = 1, shareY = TRUE, titleX = TRUE) %>%
      layout(title = paste("Coffee", toupper(coffee1), "vs Coffee", toupper(coffee2), "Rating Comparison"),
             boxmode = "group")
    
    return(p)
  })
  
  # Age distribution (plotly)
  output$agePlotly <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"age" %in% names(df)) {
      return(NULL)
    }
    
    # Age order
    age_order <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", ">65")
    
    # Prepare data
    plot_data <- df %>%
      count(age) %>%
      arrange(desc(n))
    
    # Create plot
    p <- plot_ly(plot_data, x = ~age, y = ~n, type = "bar",
                 marker = list(color = "#3c8dbc")) %>%
      layout(title = paste("Age Distribution -", filter_desc()),
             xaxis = list(title = "Age Group"),
             yaxis = list(title = "Count"))
    
    return(p)
  })
  
  # Gender distribution (plotly)
  output$genderPlotly <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"gender" %in% names(df)) {
      return(NULL)
    }
    
    # Prepare data
    plot_data <- df %>%
      count(gender) %>%
      arrange(desc(n)) %>%
      head(6)
    
    # Create plot
    p <- plot_ly(plot_data, labels = ~gender, values = ~n, type = "pie",
                 marker = list(colors = viridis::viridis(nrow(plot_data)))) %>%
      layout(title = paste("Gender Distribution -", filter_desc()))
    
    return(p)
  })
  
  # Education level distribution (plotly)
  output$educationPlotly <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"education_level" %in% names(df)) {
      return(NULL)
    }
    
    # Prepare data
    plot_data <- df %>%
      count(education_level) %>%
      arrange(desc(n))
    
    # Create plot
    p <- plot_ly(plot_data, y = ~reorder(education_level, n), x = ~n, type = "bar", orientation = 'h',
                 marker = list(color = "#00a65a")) %>%
      layout(title = paste("Education Level Distribution -", filter_desc()),
             xaxis = list(title = "Count"),
             yaxis = list(title = ""))
    
    return(p)
  })
  
  # Demographics vs. Coffee Preference
  output$demographicPreferences <- renderPlot({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"age" %in% names(df) || !"gender" %in% names(df) ||
        !"favorite_coffee" %in% names(df)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "No data available or missing required columns") +
               theme_void())
    }
    
    # Prepare data - most popular coffee by demographic groups
    age_pref <- df %>%
      group_by(age, favorite_coffee) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(age) %>%
      filter(count == max(count)) %>%
      ungroup() %>%
      mutate(demographic = "Age",
             group = age,
             top_coffee = favorite_coffee,
             count = count)
    
    gender_pref <- df %>%
      filter(gender %in% c("Male", "Female", "Non-binary")) %>% # Filter common genders
      group_by(gender, favorite_coffee) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(gender) %>%
      filter(count == max(count)) %>%
      ungroup() %>%
      mutate(demographic = "Gender",
             group = gender,
             top_coffee = favorite_coffee,
             count = count)
    
    # Combine the data
    plot_data <- bind_rows(age_pref, gender_pref) %>%
      select(demographic, group, top_coffee, count)
    
    # Create plot
    ggplot(plot_data, aes(x = group, y = count, fill = top_coffee)) +
      geom_col() +
      facet_wrap(~demographic, scales = "free_x") +
      scale_fill_viridis_d() +
      labs(
        title = "Most Popular Coffee by Demographic Group",
        subtitle = paste("Filters:", filter_desc()),
        x = "",
        y = "Count",
        fill = "Top Coffee"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")
      )
  })
  
  # Overall Coffee Preference Distribution
  output$overallPreference <- renderPlot({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !"prefer_overall" %in% names(df)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "No data available or missing 'prefer_overall' column") +
               theme_void())
    }
    
    # Prepare data
    plot_data <- df %>%
      count(prefer_overall) %>%
      arrange(desc(n))
    
    # Create plot
    ggplot(plot_data, aes(x = "", y = n, fill = prefer_overall)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_viridis_d() +
      labs(
        title = "Overall Coffee Preference Distribution",
        subtitle = paste("Filters:", filter_desc()),
        fill = "Coffee"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })
  
  # Rating Correlation Matrix
  output$ratingCorrelation <- renderPlot({
    df <- filtered_data()
    
    # Define rating columns
    rating_cols <- c(
      "coffee_a_bitterness", "coffee_a_acidity", "coffee_a_personal_preference",
      "coffee_b_bitterness", "coffee_b_acidity", "coffee_b_personal_preference",
      "coffee_c_bitterness", "coffee_c_acidity", "coffee_c_personal_preference",
      "coffee_d_bitterness", "coffee_d_acidity", "coffee_d_personal_preference"
    )
    
    # Check if we have data and required columns
    if (nrow(df) == 0 || !all(rating_cols %in% names(df))) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "No data available or missing coffee rating columns") +
               theme_void())
    }
    
    # Calculate correlation matrix
    cor_matrix <- cor(df[rating_cols], use = "pairwise.complete.obs")
    
    # Create correlation plot
    corrplot(cor_matrix, method = "color", type = "upper",
             tl.col = "black", tl.srt = 45, tl.cex = 0.8,
             title = "Correlation Between Coffee Ratings",
             mar = c(0, 0, 2, 0))
  })
  
  # Rating Distribution
  output$ratingDistribution <- renderPlot({
    df <- filtered_data()
    
    # Define types and coffees
    rating_types <- c("bitterness", "acidity", "personal_preference")
    coffees <- c("a", "b", "c", "d")
    
    # Create column names combinations
    rating_cols <- expand.grid(coffee = coffees, type = rating_types) %>%
      mutate(col_name = paste0("coffee_", coffee, "_", type))
    
    # Check if we have data and at least some required columns
    if (nrow(df) == 0 || !any(rating_cols$col_name %in% names(df))) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "No data available or missing coffee rating columns") +
               theme_void())
    }
    
    # Filter to available columns
    available_cols <- rating_cols$col_name[rating_cols$col_name %in% names(df)]
    
    # Prepare data
    plot_data <- df %>%
      select(all_of(available_cols)) %>%
      pivot_longer(
        cols = everything(),
        names_to = c("coffee", "attribute"),
        names_pattern = "coffee_([a-d])_(.*)",
        values_to = "rating"
      ) %>%
      mutate(
        coffee = paste("Coffee", toupper(coffee)),
        attribute = case_when(
          attribute == "bitterness" ~ "Bitterness",
          attribute == "acidity" ~ "Acidity",
          attribute == "personal_preference" ~ "Personal Preference"
        )
      )
    
    # Create plot
    ggplot(plot_data, aes(x = rating, y = coffee, fill = coffee)) +
      geom_density_ridges(alpha = 0.7) +
      facet_wrap(~attribute, scales = "free_x") +
      scale_fill_viridis_d() +
      labs(
        title = "Rating Distribution Across All Coffees",
        subtitle = paste("Filters:", filter_desc()),
        x = "Rating",
        y = ""
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold")
      )
  })
  
  # Advanced Analysis: Correlation Plot
  output$correlationPlot <- renderPlot({
    df <- filtered_data()
    
    # Define variables for correlation
    cor_vars <- c(
      "coffee_a_bitterness", "coffee_a_acidity", "coffee_a_personal_preference",
      "coffee_b_bitterness", "coffee_b_acidity", "coffee_b_personal_preference",
      "coffee_c_bitterness", "coffee_c_acidity", "coffee_c_personal_preference",
      "coffee_d_bitterness", "coffee_d_acidity", "coffee_d_personal_preference"
    )
    
    # Check if expertise column exists and add it
    if ("expertise" %in% names(df)) {
      cor_vars <- c(cor_vars, "expertise")
    }
    
    # Check if we have data and required columns
    if (nrow(df) == 0 || !any(cor_vars %in% names(df))) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "No data available or missing required columns") +
               theme_void())
    }
    
    # Filter to available columns
    available_vars <- cor_vars[cor_vars %in% names(df)]
    
    # Calculate correlation matrix
    cor_matrix <- cor(df[available_vars], use = "pairwise.complete.obs")
    
    # Create correlation plot with better labels
    corrplot(cor_matrix, method = "circle", type = "upper",
             tl.col = "black", tl.srt = 45, tl.cex = 0.7,
             title = "Coffee Rating Correlation Analysis",
             mar = c(0, 0, 2, 0))
  })
  
  # Correlation Insights
  output$correlationInsights <- renderUI({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(HTML("<p>No data available for insights</p>"))
    }
    
    # Generate insights based on correlations
    insights <- "<h4>Correlation Analysis Insights:</h4>"
    
    # Check for correlations between bitterness and preference
    if (all(c("coffee_a_bitterness", "coffee_a_personal_preference") %in% names(df))) {
      cor_bitter_pref <- cor(df$coffee_a_bitterness, df$coffee_a_personal_preference, use = "pairwise.complete.obs")
      
      insights <- paste0(insights, "<p>The correlation between <b>bitterness</b> and <b>personal preference</b> for Coffee A is <b>",
                         round(cor_bitter_pref, 2), "</b>, indicating a ",
                         ifelse(cor_bitter_pref > 0.3, "positive relationship (participants tend to like more bitter coffee)",
                                ifelse(cor_bitter_pref < -0.3, "negative relationship (participants tend to dislike more bitter coffee)",
                                       "weak or no relationship")),
                         ".</p>")
    }
    
    # Check for correlations between acidity and preference
    if (all(c("coffee_a_acidity", "coffee_a_personal_preference") %in% names(df))) {
      cor_acid_pref <- cor(df$coffee_a_acidity, df$coffee_a_personal_preference, use = "pairwise.complete.obs")
      
      insights <- paste0(insights, "<p>The correlation between <b>acidity</b> and <b>personal preference</b> for Coffee A is <b>",
                         round(cor_acid_pref, 2), "</b>, suggesting that ",
                         ifelse(cor_acid_pref > 0.3, "higher acidity is generally preferred",
                                ifelse(cor_acid_pref < -0.3, "lower acidity is generally preferred",
                                       "acidity has little impact on preference")),
                         ".</p>")
    }
    
    # Check for correlations between expertise and preference consistency
    if ("expertise" %in% names(df) &&
        all(c("coffee_a_personal_preference", "coffee_b_personal_preference",
              "coffee_c_personal_preference", "coffee_d_personal_preference") %in% names(df))) {
      
      # Calculate preference consistency (lower std dev = more consistent)
      df$pref_consistency <- apply(df[c("coffee_a_personal_preference",
                                        "coffee_b_personal_preference",
                                        "coffee_c_personal_preference",
                                        "coffee_d_personal_preference")], 1, sd, na.rm = TRUE)
      
      cor_exp_consistency <- cor(df$expertise, df$pref_consistency, use = "pairwise.complete.obs")
      
      insights <- paste0(insights, "<p>The correlation between <b>self-reported expertise</b> and <b>preference consistency</b> is <b>",
                         round(cor_exp_consistency, 2), "</b>, which suggests that people with ",
                         ifelse(cor_exp_consistency < -0.2, "higher expertise tend to have more consistent preferences",
                                ifelse(cor_exp_consistency > 0.2, "higher expertise actually have more varied preferences",
                                       "expertise level doesn't strongly relate to consistency in preferences")),
                         ".</p>")
    }
    
    HTML(insights)
  })
  
  # Advanced Analysis: PCA Plot
  output$pcaPlot <- renderPlot({
    df <- filtered_data()
    
    # Define variables for PCA
    pca_vars <- c(
      "coffee_a_bitterness", "coffee_a_acidity", "coffee_a_personal_preference",
      "coffee_b_bitterness", "coffee_b_acidity", "coffee_b_personal_preference",
      "coffee_c_bitterness", "coffee_c_acidity", "coffee_c_personal_preference",
      "coffee_d_bitterness", "coffee_d_acidity", "coffee_d_personal_preference"
    )
    
    # Check if we have data and required columns
    if (nrow(df) == 0 || !all(pca_vars %in% names(df))) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "No data available or missing required columns for PCA") +
               theme_void())
    }
    
    # Run PCA
    tryCatch({
      pca_result <- prcomp(df[pca_vars], scale. = TRUE)
      
      # Get the individual observations in principal component space
      pca_data <- as.data.frame(pca_result$x)
      
      # Add group information if available
      if ("favorite_coffee" %in% names(df)) {
        pca_data$favorite_coffee <- df$favorite_coffee
      } else {
        pca_data$favorite_coffee <- "Unknown"
      }
      
      # Create plot
      ggplot(pca_data, aes(x = PC1, y = PC2, color = favorite_coffee)) +
        geom_point(alpha = 0.7) +
        scale_color_viridis_d() +
        labs(
          title = "Principal Component Analysis of Coffee Ratings",
          subtitle = "First two principal components",
          x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "% variance)"),
          y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "% variance)"),
          color = "Favorite Coffee"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold")
        )
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Error during PCA:", e$message)) +
        theme_void()
    })
  })
  
  # PCA Insights
  output$pcaInsights <- renderUI({
    df <- filtered_data()
    
    # Define variables for PCA
    pca_vars <- c(
      "coffee_a_bitterness", "coffee_a_acidity", "coffee_a_personal_preference",
      "coffee_b_bitterness", "coffee_b_acidity", "coffee_b_personal_preference",
      "coffee_c_bitterness", "coffee_c_acidity", "coffee_c_personal_preference",
      "coffee_d_bitterness", "coffee_d_acidity", "coffee_d_personal_preference"
    )
    
    # Check if we have data and required columns
    if (nrow(df) == 0 || !all(pca_vars %in% names(df))) {
      return(HTML("<p>Insufficient data for PCA analysis</p>"))
    }
    
    # Run PCA
    tryCatch({
      pca_result <- prcomp(df[pca_vars], scale. = TRUE)
      
      # Get loadings
      loadings <- as.data.frame(pca_result$rotation)
      
      # Generate insights based on loadings
      insights <- "<h4>Principal Component Analysis Insights:</h4>"
      
      # PC1 insights - find top contributing variables
      pc1_loadings <- loadings %>%
        select(PC1) %>%
        arrange(desc(abs(PC1)))
      
      top_pc1_vars <- rownames(pc1_loadings)[1:3]
      top_pc1_loadings <- pc1_loadings$PC1[1:3]
      
      insights <- paste0(insights, "<p><b>First Principal Component:</b> The primary dimension of variation (PC1) is most strongly associated with ",
                         paste(top_pc1_vars, collapse = ", "),
                         ". This suggests that the main factor distinguishing coffee preferences is related to ",
                         ifelse(grepl("bitterness", top_pc1_vars[1]), "bitterness perception",
                                ifelse(grepl("acidity", top_pc1_vars[1]), "acidity perception",
                                       "personal preference")),
                         ".</p>")
      
      # PC2 insights
      pc2_loadings <- loadings %>%
        select(PC2) %>%
        arrange(desc(abs(PC2)))
      
      top_pc2_vars <- rownames(pc2_loadings)[1:3]
      top_pc2_loadings <- pc2_loadings$PC2[1:3]
      
      insights <- paste0(insights, "<p><b>Second Principal Component:</b> The secondary dimension (PC2) is most strongly associated with ",
                         paste(top_pc2_vars, collapse = ", "),
                         ". This represents another important aspect of variation in the data, possibly related to ",
                         ifelse(grepl("bitterness", top_pc2_vars[1]), "bitterness perception",
                                ifelse(grepl("acidity", top_pc2_vars[1]), "acidity perception",
                                       "personal preference")),
                         ".</p>")
      
      # Variance explained
      variance_explained <- summary(pca_result)$importance[2, 1:2]
      
      insights <- paste0(insights, "<p>Together, the first two principal components explain <b>",
                         round(sum(variance_explained) * 100, 1),
                         "%</b> of the total variation in coffee ratings, suggesting that coffee preferences can be reasonably well-represented in a two-dimensional space.</p>")
      
      HTML(insights)
    }, error = function(e) {
      HTML(paste("<p>Error during PCA analysis:", e$message, "</p>"))
    })
  })
  
  # Advanced Analysis: Bitterness vs. Preference
  output$bitterPreference <- renderPlot({
    df <- filtered_data()
    
    # Check if we have necessary columns
    required_cols <- c("coffee_a_bitterness", "coffee_a_personal_preference",
                       "coffee_b_bitterness", "coffee_b_personal_preference",
                       "coffee_c_bitterness", "coffee_c_personal_preference")
    
    if (nrow(df) == 0 || !all(required_cols %in% names(df))) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "No data available or missing required columns") +
               theme_void())
    }
    
    # Prepare data - combine all coffees
    plot_data <- bind_rows(
      df %>% select(bitterness = coffee_a_bitterness, preference = coffee_a_personal_preference) %>% mutate(coffee = "A"),
      df %>% select(bitterness = coffee_b_bitterness, preference = coffee_b_personal_preference) %>% mutate(coffee = "B"),
      df %>% select(bitterness = coffee_c_bitterness, preference = coffee_c_personal_preference) %>% mutate(coffee = "C")
    )
    
    if ("coffee_d_bitterness" %in% names(df) && "coffee_d_personal_preference" %in% names(df)) {
      plot_data <- bind_rows(
        plot_data,
        df %>% select(bitterness = coffee_d_bitterness, preference = coffee_d_personal_preference) %>% mutate(coffee = "D")
      )
    }
    
    # Create plot
    ggplot(plot_data, aes(x = bitterness, y = preference, color = coffee)) +
      geom_jitter(alpha = 0.3, width = 0.2, height = 0.2) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
      scale_color_viridis_d() +
      facet_wrap(~coffee, ncol = 2) +
      labs(
        title = "Relationship Between Bitterness and Preference",
        subtitle = paste("Filters:", filter_desc()),
        x = "Bitterness Rating",
        y = "Preference Rating",
        color = "Coffee"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold")
      )
  })
  
  # Bitterness vs Preference Insights
  output$bitterInsights <- renderUI({
    df <- filtered_data()
    
    # Check if we have necessary columns
    required_cols <- c("coffee_a_bitterness", "coffee_a_personal_preference",
                       "coffee_b_bitterness", "coffee_b_personal_preference")
    
    if (nrow(df) == 0 || !all(required_cols %in% names(df))) {
      return(HTML("<p>Insufficient data for analysis</p>"))
    }
    
    # Generate insights based on correlations between bitterness and preference
    insights <- "<h4>Bitterness vs. Preference Insights:</h4>"
    
    # Calculate correlations for each coffee
    coffee_letters <- c("a", "b", "c", "d")
    correlations <- c()
    
    for (letter in coffee_letters) {
      bitter_col <- paste0("coffee_", letter, "_bitterness")
      pref_col <- paste0("coffee_", letter, "_personal_preference")
      
      if (bitter_col %in% names(df) && pref_col %in% names(df)) {
        cor_val <- cor(df[[bitter_col]], df[[pref_col]], use = "pairwise.complete.obs")
        correlations <- c(correlations, cor_val)
        
        insights <- paste0(insights, "<p><b>Coffee ", toupper(letter), ":</b> The correlation between bitterness and preference is <b>",
                           round(cor_val, 2), "</b>, which suggests that ",
                           ifelse(cor_val > 0.2, "higher bitterness is associated with higher preference ratings. This coffee's bitter notes are likely perceived positively.",
                                  ifelse(cor_val < -0.2, "higher bitterness is associated with lower preference ratings. This coffee may be perceived as too bitter by many participants.",
                                         "bitterness has little impact on preference for this coffee.")),
                           "</p>")
      }
    }
    
    # Add overall insight
    if (length(correlations) > 0) {
      avg_cor <- mean(correlations)
      insights <- paste0(insights, "<p><b>Overall:</b> Across all coffees, the average correlation between bitterness and preference is <b>",
                         round(avg_cor, 2), "</b>. This suggests that, in general, people in this study ",
                         ifelse(avg_cor > 0.1, "tend to appreciate some bitterness in their coffee.",
                                ifelse(avg_cor < -0.1, "tend to prefer less bitter coffee.",
                                       "don't show a strong preference pattern based on bitterness alone.")),
                         "</p>")
    }
    
    # Add expertise insight if available
    if ("expertise" %in% names(df)) {
      # Create high and low expertise groups
      median_expertise <- median(df$expertise, na.rm = TRUE)
      high_expertise <- df %>% filter(expertise > median_expertise)
      low_expertise <- df %>% filter(expertise <= median_expertise)
      
      # Compare correlations for coffee A
      if ("coffee_a_bitterness" %in% names(df) && "coffee_a_personal_preference" %in% names(df)) {
        high_cor <- cor(high_expertise$coffee_a_bitterness, high_expertise$coffee_a_personal_preference, use = "pairwise.complete.obs")
        low_cor <- cor(low_expertise$coffee_a_bitterness, low_expertise$coffee_a_personal_preference, use = "pairwise.complete.obs")
        
        insights <- paste0(insights, "<p><b>Expertise Effect:</b> The correlation between bitterness and preference for Coffee A is <b>",
                           round(high_cor, 2), "</b> for high-expertise participants and <b>",
                           round(low_cor, 2), "</b> for low-expertise participants. This suggests that ",
                           ifelse(abs(high_cor - low_cor) < 0.1, "expertise level doesn't significantly change how bitterness affects preference.",
                                  ifelse(high_cor > low_cor, "those with more coffee expertise tend to appreciate bitterness more positively.",
                                         "those with less coffee expertise tend to appreciate bitterness more positively.")),
                           "</p>")
      }
    }
    
    HTML(insights)
  })
  
  # Consumer Segments: Clustering Analysis
  # This will run when the user clicks the "Run Clustering" button
  clustering_result <- eventReactive(input$runClustering, {
    df <- filtered_data()
    
    # Define potential variables for clustering
    demo_vars <- c("expertise") # Add other suitable numeric/ordinal demo vars if available
    pref_vars <- c(
      "coffee_a_personal_preference", "coffee_b_personal_preference",
      "coffee_c_personal_preference", "coffee_d_personal_preference",
      "avg_bitterness", "avg_acidity"
    ) # Preference variables
    
    # Check which variables are actually available in the filtered data
    available_demo_vars <- demo_vars[demo_vars %in% names(df)]
    available_pref_vars <- pref_vars[pref_vars %in% names(df)]
    
    # Decide which variables to use based on user selection and availability
    cluster_vars <- c()
    if (input$clusterVars == "demo" && length(available_demo_vars) > 0) {
      cluster_vars <- available_demo_vars
    } else if (input$clusterVars == "pref" && length(available_pref_vars) > 0) {
      cluster_vars <- available_pref_vars
    } else if (input$clusterVars == "both") {
      cluster_vars <- c(available_demo_vars, available_pref_vars)
    }
    
    # Check if we have enough data and at least two variables for clustering
    if (nrow(df) < 10 || length(cluster_vars) < 2) {
      return(list(
        success = FALSE,
        message = paste("Insufficient data or variables for clustering. Need at least 10 records and 2 variables.",
                        "Records:", nrow(df), "Variables selected:", length(cluster_vars))
      ))
    }
    
    # Attempt clustering
    tryCatch({
      # Prepare data: select, convert to numeric, handle NAs, scale
      cluster_data_prep <- df %>%
        select(all_of(cluster_vars)) %>%
        mutate(across(everything(), as.numeric)) %>% # Ensure numeric
        mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) # Impute NAs
      
      # Check for zero variance columns after imputation
      constant_cols <- sapply(cluster_data_prep, function(x) var(x, na.rm = TRUE) == 0)
      if (any(constant_cols)) {
        warning(paste("Removing constant columns before scaling:",
                      paste(names(constant_cols)[constant_cols], collapse=", ")))
        cluster_data_prep <- cluster_data_prep[, !constant_cols]
        cluster_vars <- names(cluster_data_prep) # Update used variables
        if(length(cluster_vars) < 2) {
          return(list(success = FALSE, message = "Not enough non-constant variables left for clustering."))
        }
      }
      
      cluster_data_scaled <- scale(cluster_data_prep)
      
      # Run k-means clustering
      k <- as.numeric(input$numClusters)
      set.seed(123) # For reproducibility
      kmeans_result <- kmeans(cluster_data_scaled, centers = k, nstart = 25)
      
      # Add cluster assignments to original data
      df$cluster <- factor(kmeans_result$cluster)
      
      # Compute cluster profiles using original (unscaled) data where appropriate
      profile_vars_to_summarize <- c("cluster", cluster_vars) # Start with clustering vars
      # Add other potentially interesting numeric vars if they exist
      other_numeric_vars <- c("expertise", "avg_pref_score", "avg_bitterness", "avg_acidity",
                              "coffee_a_bitterness", "coffee_a_acidity", "coffee_a_personal_preference",
                              "coffee_b_bitterness", "coffee_b_acidity", "coffee_b_personal_preference",
                              "coffee_c_bitterness", "coffee_c_acidity", "coffee_c_personal_preference",
                              "coffee_d_bitterness", "coffee_d_acidity", "coffee_d_personal_preference")
      available_other_numeric <- other_numeric_vars[other_numeric_vars %in% names(df)]
      profile_vars_to_summarize <- unique(c(profile_vars_to_summarize, available_other_numeric))
      
      
      cluster_profiles <- df %>%
        select(all_of(profile_vars_to_summarize)) %>% # Select relevant vars
        group_by(cluster) %>%
        summarise(
          count = n(),
          percentage = n() / nrow(df) * 100,
          # Summarize all selected numeric variables
          across(where(is.numeric), ~mean(., na.rm = TRUE))
        )
      
      return(list(
        success = TRUE,
        data = df, # Data with cluster assignments
        clusters = kmeans_result, # K-means object
        profiles = cluster_profiles, # Summary table
        vars_used = cluster_vars # Actual variables used
      ))
    }, error = function(e) {
      return(list(
        success = FALSE,
        message = paste("Error during clustering:", e$message)
      ))
    })
  })
  
  
  # Clustering Visualization
  output$segmentPlot <- renderPlot({
    result <- clustering_result()
    
    if (!result$success) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = result$message) +
               theme_void())
    }
    
    # Get data with cluster assignments
    df <- result$data
    
    # Get variables used for clustering
    vars_used <- result$vars_used
    
    if (length(vars_used) < 2) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "Need at least 2 variables for visualization") +
               theme_void())
    }
    
    # Run PCA on the clustering variables for visualization
    cluster_data_vis <- df %>%
      select(all_of(vars_used)) %>%
      mutate(across(everything(), as.numeric)) %>%
      mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
    
    # Scale data and run PCA
    pca_result_vis <- prcomp(cluster_data_vis, scale. = TRUE)
    
    # Get the first two principal components
    pca_data_vis <- as.data.frame(pca_result_vis$x[, 1:2])
    pca_data_vis$cluster <- df$cluster # Add cluster assignments from result
    
    # Create visualization
    ggplot(pca_data_vis, aes(x = PC1, y = PC2, color = cluster)) +
      geom_point(alpha = 0.7, size = 3) +
      stat_ellipse(type = "norm", level = 0.68) + # Add ellipses around clusters
      scale_color_viridis_d() +
      labs(
        title = paste(input$numClusters, "Coffee Consumer Segments"),
        subtitle = paste("Based on", input$clusterVars, "variables;",
                         "Plotted using first two principal components"),
        x = paste0("Principal Component 1 (", round(summary(pca_result_vis)$importance[2, 1] * 100, 1), "% variance)"),
        y = paste0("Principal Component 2 (", round(summary(pca_result_vis)$importance[2, 2] * 100, 1), "% variance)"),
        color = "Segment"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold")
      )
  })
  
  
  # Segment Profiles Table
  output$segmentProfiles <- DT::renderDataTable({
    result <- clustering_result()
    
    if (!result$success) {
      # Return a data frame with the error message for DT
      return(DT::datatable(data.frame(Error = result$message), options = list(dom = 't')))
    }
    
    # Get segment profiles
    profiles <- result$profiles
    
    # Format percentages
    profiles$percentage <- round(profiles$percentage, 1)
    
    # Rename columns for better display (handle potential missing columns gracefully)
    profiles <- profiles %>%
      rename_with(~gsub("^coffee_([a-d])_", "Coffee \\1 ", .), matches("^coffee_[a-d]_")) %>%
      rename_with(~gsub("_", " ", .), everything()) %>%
      rename_with(~tools::toTitleCase(.), everything()) # Title case for readability
    
    # Round numeric columns (except count and percentage which might be handled differently)
    profiles <- profiles %>%
      mutate(across(where(is.numeric) & !matches("Count|Percentage"), ~round(., 2)))
    
    # Rename 'Cluster' column
    if("Cluster" %in% names(profiles)){
      profiles <- profiles %>% rename(`Segment` = `Cluster`)
      profiles$Segment <- paste("Segment", profiles$Segment)
    }
    
    
    DT::datatable(profiles,
                  options = list(
                    pageLength = 5,
                    scrollX = TRUE,
                    searching = FALSE, # Disable search for profile table
                    info = FALSE, # Hide 'Showing X entries'
                    lengthChange = FALSE # Hide show entries dropdown
                  ),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: left; color:black; font-size:1.2em;',
                    'Consumer Segment Profiles (Average Values)'
                  )) %>%
      # Optional: Add color bars for count/percentage
      DT::formatStyle(
        'Count',
        background = DT::styleColorBar(c(0, max(profiles$Count, na.rm=TRUE)), '#b8daff'),
        backgroundSize = '90% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        'Percentage',
        background = DT::styleColorBar(c(0, 100), '#d4edda'), # Greenish for percentage
        backgroundSize = '90% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Cluster Summary Text
  output$clusterSummary <- renderUI({
    result <- clustering_result()
    
    if (!result$success) {
      return(HTML(paste("<p class='text-danger'><b>Clustering Error:</b>", result$message, "</p>")))
    }
    
    # Get segment profiles and data
    profiles <- result$profiles
    df <- result$data # Data with cluster assignments
    
    # Generate interpretable summary
    summary_text <- "<h4>Consumer Segments Summary</h4>"
    
    # Add total number of profiles
    summary_text <- paste0(summary_text, "<p>Successfully identified <b>", nrow(profiles),
                           " segments</b> among ", nrow(df), " participants.</p>")
    
    # Add variables used
    summary_text <- paste0(summary_text, "<p>Variables used for segmentation: <b>",
                           paste(gsub("_", " ", result$vars_used), collapse = ", "), "</b></p>")
    
    # Describe each segment based on profile averages
    summary_text <- paste0(summary_text, "<ul>")
    
    # Rename profile columns for easier access
    profiles_renamed <- profiles %>%
      rename_with(~gsub("^coffee_([a-d])_", "coffee_\\1_", .), matches("^coffee_[a-d]_")) %>%
      rename_with(~gsub(" ", "_", .), everything()) %>% # Replace spaces with underscores
      rename_with(~tolower(.), everything()) # Lowercase all
    
    for (i in 1:nrow(profiles_renamed)) {
      segment <- profiles_renamed[i, ]
      
      # Construct segment description
      description <- paste0("<li><b>Segment ", segment$cluster, " (", round(segment$percentage,1), "%)</b>: ")
      description_parts <- c() # Store parts of the description
      
      # Add expertise if available
      if ("expertise" %in% colnames(segment)) {
        expertise_level <- segment$expertise
        desc_part <- paste0("Avg. Expertise: ",
                            ifelse(expertise_level > 7, "High",
                                   ifelse(expertise_level > 4, "Medium", "Low")),
                            " (", round(expertise_level, 1), ")")
        description_parts <- c(description_parts, desc_part)
      }
      
      # Add coffee preferences if available
      pref_cols <- grep("coffee_[a-d]_personal_preference", colnames(segment), value = TRUE)
      if (length(pref_cols) > 0) {
        pref_values <- unlist(segment[pref_cols])
        names(pref_values) <- toupper(gsub("coffee_|_personal_preference", "", names(pref_values))) # Get A, B, C, D
        max_pref_letter <- names(pref_values)[which.max(pref_values)]
        min_pref_letter <- names(pref_values)[which.min(pref_values)]
        desc_part <- paste0("Prefers Coffee ", max_pref_letter, " (", round(max(pref_values), 1), ")",
                            ", Least likes Coffee ", min_pref_letter, " (", round(min(pref_values), 1), ")")
        description_parts <- c(description_parts, desc_part)
      }
      
      # Add bitterness/acidity sensitivity if available
      if ("avg_bitterness" %in% colnames(segment)) {
        bitter_level <- segment$avg_bitterness
        desc_part <- paste0("Avg. Bitterness Rating: ", round(bitter_level, 1),
                            " (", ifelse(bitter_level > 7, "High", ifelse(bitter_level > 4, "Medium", "Low")), " Sensitivity)")
        description_parts <- c(description_parts, desc_part)
      }
      if ("avg_acidity" %in% colnames(segment)) {
        acid_level <- segment$avg_acidity
        desc_part <- paste0("Avg. Acidity Rating: ", round(acid_level, 1),
                            " (", ifelse(acid_level > 7, "High", ifelse(acid_level > 4, "Medium", "Low")), " Sensitivity)")
        description_parts <- c(description_parts, desc_part)
      }
      
      # Combine description parts
      description <- paste0(description, paste(description_parts, collapse = "; "), ".")
      description <- paste0(description, "</li>")
      summary_text <- paste0(summary_text, description)
    }
    
    summary_text <- paste0(summary_text, "</ul>")
    
    HTML(summary_text)
  })
}

# Run the application
shinyApp(ui = ui, server = server)