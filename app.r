library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(lubridate)
library(ggplot2)
library(tidyr)
library(viridis)

# Load all CSVs from /data folder
load_data <- function() {
  files <- list.files("data", full.names = TRUE, pattern = "*.csv")
  data <- lapply(files, function(file) {
    df <- read.csv(file)
    
    # Standardize column types
    if ("AvgCH" %in% colnames(df)) {
      df$AvgCH <- as.numeric(as.character(df$AvgCH))  # Convert AvgCH to numeric
    }
    
    # Extract season and country info from file names
    season <- gsub(".*_(\\d{4})\\.csv", "\\1", file)
    country_league <- gsub("_\\d{4}\\.csv", "", basename(file))
    country <- switch(substr(country_league, 1, 2),
                      "pr" = "England",
                      "la" = "Spain",
                      "bu" = "Germany",
                      "se" = "Italy",
                      "li" = "France",
                      "Unknown")
    division <- switch(country_league,
                       "premierleague" = "Premier League",
                       "laliga" = "La Liga",
                       "bundesliga" = "Bundesliga",
                       "seriea" = "Serie A",
                       "ligue1" = "Ligue 1",
                       "Unknown")
    
    df$Season <- season
    df$Country <- country
    df$Division <- division
    df
  })
  bind_rows(data)
}

# Load the data
df_all <- load_data()
df_all$Date <- as.Date(df_all$Date, format = "%d/%m/%Y")  # adjust if needed


df_all <- load_data()
df_all$Date <- as.Date(df_all$Date, format = "%d/%m/%Y")  # adjust if needed

ui <- dashboardPage(
  dashboardHeader(title = "Football Stats Dashboard", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Application", tabName = "application", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Welcome Page
      tabItem(tabName = "welcome",
              tags$head(
                tags$style(HTML("
            .navbar {
      background-color: #000000 !important;
    }
    
    /* Adjust the title of the header to center it */
    .navbar .logo {
      background-color: transparent !important;
      text-align: center !important;
      width: 100%;
    }
    .navbar .navbar-header {
      width: 100%;
      text-align: center;
    }
    .welcome-page {
              position: relative;
              background-image: url('stadium-pic.jpg');
              background-size: cover;
              background-position: center;
              height: 100vh;
              color: white;
              text-align: center;
              padding-top: 15%;
              font-family: 'Georgia', serif;
            }
            .welcome-overlay {
              position: absolute;
              top: 0; left: 0; right: 0; bottom: 0;
              background-color: rgba(0, 0, 0, 0.6);
              z-index: 1;
            }
            .welcome-content {
              position: relative;
              z-index: 2;
            }
            .welcome-title {
              font-size: 60px;
              font-weight: bold;
              margin-bottom: 10px;
            }
            .welcome-subtitle {
              font-size: 22px;
              margin-bottom: 20px;
            }
            .flags {
              margin: 20px auto;
            }
            .flags img {
              height: 30px;
              margin: 0 5px;
            }
            .wiki-btn {
              margin-top: 20px;
              padding: 10px 20px;
              font-size: 16px;
              background-color: #ffffff;
              color: #000;
              border: none;
              border-radius: 5px;
              text-decoration: none;
            }
            .footer-note {
  position: absolute;
  bottom: 10px;
  width: 100%;
  text-align: center;
  font-size: 14px; /* increased font size */
  color: #ffffff; /* bright white */
  background-color: rgba(0, 0, 0, 0.3); /* more transparent background */
  padding: 12px 0; /* slight increase for balance */
  box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.7); /* soft glow */
}


          "))
              ),
              
              div(class = "welcome-page",
                  div(class = "welcome-overlay"),
                  div(class = "welcome-content",
                      div(class = "welcome-title", "Football Stats Dashboard"),
                      div(class = "welcome-subtitle", "A Portal to Detailed European Football Statistics"),
                      tags$hr(style = "width: 100px; border-top: 3px double #fff;"),
                      
                      div(class = "flags",
                          lapply(c("england", "germany", "spain", "france", "italy"), function(flag) {
                            tags$img(src = paste0("flags/", flag, ".png"))
                            
                            
                          })
                      ),
                      
                      p("Click the Application button at the left top to begin"),
                      
                  ),
                  
                  div(class = "footer-note",
                      HTML("Developed in R and Rstudio.")
                  )
              )
      )
      ,
      
      
      # Application Page
      tabItem(tabName = "application",
              fluidRow(
                box(selectInput("country", "Select Country:", choices = unique(df_all$Country))),
                box(selectInput("division", "Select Division:", choices = unique(df_all$Division))),
                box(selectInput("season", "Select Season:", choices = unique(df_all$Season))),
                box(dateRangeInput("daterange", "Select Date Range:",
                                   start = min(df_all$Date, na.rm = TRUE),
                                   end = max(df_all$Date, na.rm = TRUE)))
              ),
              tabBox(width = 12,
                     tabPanel("League Table", DTOutput("league_table")),
                     tabPanel("Goals", DTOutput("goals_table")),
                     tabPanel("Results", DTOutput("results_table")),
                     tabPanel("Over / Under", DTOutput("ou_table")),
                     tabPanel("Cards / Fouls", DTOutput("cards_table")),
                     tabPanel("Corners", DTOutput("corners_table")),
                     tabPanel("Raw Table", DTOutput("raw_table"))
              ),
              
              # Add visualizations below the tables
              fluidRow(
                box(width = 6, 
                    title = "Goals per Team", 
                    solidHeader = TRUE, 
                    status = "primary",
                    plotOutput("goals_chart")
                ),
                box(width = 6, 
                    title = "Matches Result Distribution", 
                    solidHeader = TRUE, 
                    status = "primary",
                    plotOutput("results_chart")
                )
              ),
              fluidRow(
                box(width = 6, 
                    title = "Over 2.5 Goals", 
                    solidHeader = TRUE, 
                    status = "primary",
                    plotOutput("over_under_chart")
                ),
                box(width = 6, 
                    title = "Yellow and Red Cards Distribution", 
                    solidHeader = TRUE, 
                    status = "primary",
                    plotOutput("cards_chart")
                )
              ),
              fluidRow(
                box(width = 6,
                    title = "Average Corners per Team",
                    solidHeader = TRUE,
                    status = "info",
                    plotOutput("corners_chart")
                ),
                
                # You can pair this with something else or leave the second box for future expansion
                box(width = 6,
                    title = "Home vs Away Wins",
                    solidHeader = TRUE,
                    status = "warning",
                    plotOutput("home_away_chart")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Fouls per Match Over Time",
                    solidHeader = TRUE,
                    status = "success",
                    plotOutput("fouls_trend_chart")
                )
              )
      )
      
      ,
      
      # About Page
      tabItem(tabName = "about",
              tags$head(
                tags$style(HTML("
            .about-page {
              position: relative;
              background-image: url('about_page_image.jpg');
              background-size: cover;
              background-position: center;
              min-height: 100vh;
              padding: 40px;
              color: #ffffff;
              font-family: 'Georgia', serif;
            }
            .about-overlay {
              position: absolute;
              top: 0; left: 0; right: 0; bottom: 0;
              background-color: rgba(0, 0, 0, 0.6);
              z-index: 1;
            }
            .about-content {
              position: relative;
              z-index: 2;
              background-color: rgba(0, 0, 0, 0.5);
              padding: 30px;
              border-radius: 10px;
              box-shadow: 0 0 10px rgba(0,0,0,0.8);
            }
            .about-content h2 {
              font-size: 36px;
              margin-bottom: 20px;
              color: #00ffcc;
              border-bottom: 2px solid #00ffcc;
              padding-bottom: 5px;
            }
            .about-content h3 {
              font-size: 26px;
              margin-top: 30px;
              color: #ffd700;
            }
            .about-content p {
              font-size: 16px;
              line-height: 1.6;
              color: #f0f0f0;
            }
            .about-content a {
              color: #66ccff;
              text-decoration: underline;
            }
          "))
              ),
              div(class = "about-page",
                  div(class = "about-overlay"),
                  div(class = "about-content",
                      h2("About This Dashboard"),
                      h3("Data"),
                      p("The data in this application comes from and is owned by ",
                        tags$a(href = "https://www.football-data.co.uk/", target = "_blank", "football-data.co.uk"),
                        ", a free football and betting database. Football-data offers many more services than just data. Feel free to pay them a visit!"
                      ),
                      
                      h3("Graphs"),
                      p("The graphs in this application come from the ",
                        tags$a(href = "https://www.highcharts.com/", target = "_blank", "Highcharts JavaScript graphics library"),
                        ". Visit their website for more information!"
                      ),
                      
                      h3("Get in Touch!"),
                      p("If you would like to contact the author about the application feel free to drop him an email at: ",
                        tags$b("sumitpandey@myyahoo.com"),
                        " or connect him on LinkedIn at ",
                        tags$a(href = "https://www.linkedin.com/in/5umitpandey", target = "_blank", "https://www.linkedin.com/in/5umitpandey")
                      ),
                      
                      h3("Issues"),
                      p("If you found an issue or bug with the application please do post it as such on the development page, under the issues tab in order to have a look.")
                  )
              )
      )
      
    )
  )
)

# Server Function
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- df_all %>%
      filter(Country == input$country,
             Division == input$division,
             Season == input$season,
             Date >= input$daterange[1],
             Date <= input$daterange[2])
    df
  })
  
  
  # Update the divisions dynamically based on the selected country
  observe({
    country_selected <- input$country
    
    # Update division choices based on the selected country
    divisions <- unique(df_all$Division[df_all$Country == country_selected])
    
    # Update the division input choices
    updateSelectInput(session, "division", choices = divisions)
  })
  
  observeEvent(input$season, {
    selected_season <- input$season
    
    # Get the start and end year based on the selected season (e.g., 2024)
    start_year <- as.integer(selected_season)
    end_year <- start_year + 1
    
    # Define the start and end dates based on the selected season
    start_date <- as.Date(paste0(start_year, "-06-01"))
    end_date <- as.Date(paste0(end_year, "-08-31"))
    
    # Update the dateRangeInput UI
    updateDateRangeInput(session, "daterange", 
                         start = start_date,
                         end = end_date,
                         min = start_date,
                         max = end_date)
  })
  
  # League Table Calculation
  output$league_table <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    
    home <- df %>%
      group_by(Team = HomeTeam) %>%
      summarise(P = n(),
                W = sum(FTR == "H"),
                D = sum(FTR == "D"),
                L = sum(FTR == "A"),
                F = sum(FTHG),
                A = sum(FTAG),
                .groups = 'drop')
    
    away <- df %>%
      group_by(Team = AwayTeam) %>%
      summarise(P = n(),
                W = sum(FTR == "A"),
                D = sum(FTR == "D"),
                L = sum(FTR == "H"),
                F = sum(FTAG),
                A = sum(FTHG),
                .groups = 'drop')
    
    league <- full_join(home, away, by = "Team", suffix = c(".home", ".away")) %>%
      mutate(
        P = P.home + P.away,
        W = W.home + W.away,
        D = D.home + D.away,
        L = L.home + L.away,
        F = F.home + F.away,
        A = A.home + A.away,
        GD = F - A,
        Pts = W * 3 + D
      ) %>%
      select(Team, P, W, D, L, F, A, GD, Pts) %>%
      arrange(desc(Pts), desc(GD), desc(F))
    
    datatable(league, options = list(pageLength = 20))
  })
  
  # Other tables
  output$goals_table <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    df %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$results_table <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    df %>% select(Date, HomeTeam, AwayTeam, FTR) %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$ou_table <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    df %>% mutate(TotalGoals = FTHG + FTAG,
                  Over2.5 = ifelse(TotalGoals > 2.5, "Yes", "No")) %>%
      select(Date, HomeTeam, AwayTeam, TotalGoals, Over2.5) %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$cards_table <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    df %>% select(Date, HomeTeam, AwayTeam, HY, AY, HR, AR) %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$corners_table <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    df %>% select(Date, HomeTeam, AwayTeam, HC, AC) %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$raw_table <- renderDT({
    df <- filtered_data()
    datatable(df, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Goal Distribution Chart
  output$goals_chart <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    
    goals_data <- df %>%
      group_by(Team = HomeTeam) %>%
      summarise(TotalGoals = sum(FTHG)) %>%
      arrange(desc(TotalGoals))
    
    ggplot(goals_data, aes(x = reorder(Team, TotalGoals), y = TotalGoals, fill = Team)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Total Goals per Team", x = "Team", y = "Total Goals") +
      scale_fill_viridis(discrete = TRUE)
  })
  
  
  # Match Result Distribution Chart
  output$results_chart <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    
    result_data <- df %>%
      group_by(FTR) %>%
      summarise(Count = n())
    
    ggplot(result_data, aes(x = FTR, y = Count, fill = FTR)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Match Result Distribution", x = "Result", y = "Count") +
      scale_fill_manual(values = c("green", "yellow", "red"))
  })
  
  # Over 2.5 Goals Distribution Chart
  output$over_under_chart <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    
    over_under_data <- df %>%
      mutate(TotalGoals = FTHG + FTAG, Over2.5 = ifelse(TotalGoals > 2.5, "Yes", "No")) %>%
      group_by(Over2.5) %>%
      summarise(Count = n())
    
    ggplot(over_under_data, aes(x = Over2.5, y = Count, fill = Over2.5)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Over 2.5 Goals", x = "Over 2.5", y = "Count") +
      scale_fill_manual(values = c("skyblue", "lightcoral"))
  })
  
  # Home vs Away Wins
  output$home_away_chart <- renderPlot({
    df_filtered <- df_all %>%
      filter(Country == input$country,
             Division == input$division,
             Season == input$season,
             Date >= input$daterange[1],
             Date <= input$daterange[2])
    
    df_results <- df_filtered %>%
      mutate(Result = case_when(
        FTHG > FTAG ~ "Home Win",
        FTHG < FTAG ~ "Away Win",
        TRUE ~ "Draw"
      )) %>%
      count(Result)
    
    ggplot(df_results, aes(x = "", y = n, fill = Result)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      scale_fill_manual(values = c("Home Win" = "#00BA38", "Away Win" = "#F8766D", "Draw" = "#619CFF")) +
      labs(title = "Home vs Away Wins") +
      theme_void()
  })
  
  # Cards Distribution Chart
  output$cards_chart <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    
    card_data <- df %>%
      summarise(TotalYellowCards = sum(HY + AY), TotalRedCards = sum(HR + AR))
    
    card_data %>%
      gather(CardType, Count, TotalYellowCards, TotalRedCards) %>%
      ggplot(aes(x = CardType, y = Count, fill = CardType)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Yellow and Red Cards Distribution", x = "Card Type", y = "Count") +
      scale_fill_manual(values = c("yellow", "red"))
  })
  
  # Corner kicks per team
  output$corners_chart <- renderPlot({
    df_filtered <- df_all %>%
      filter(Country == input$country,
             Division == input$division,
             Season == input$season,
             Date >= input$daterange[1],
             Date <= input$daterange[2]) %>%
      group_by(HomeTeam) %>%
      summarise(AvgCorners = mean(HC + AC, na.rm = TRUE)) %>%
      arrange(desc(AvgCorners))
    
    ggplot(df_filtered, aes(x = reorder(HomeTeam, AvgCorners), y = AvgCorners, fill = AvgCorners)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_gradient(low = "#FFDDC1", high = "#FF5733") +
      labs(title = "Average Corners per Team", x = "Team", y = "Avg Corners") +
      theme_minimal()
  })
  
  # Fouls trend chart
  output$fouls_trend_chart <- renderPlot({
    df_filtered <- df_all %>%
      filter(Country == input$country,
             Division == input$division,
             Season == input$season,
             Date >= input$daterange[1],
             Date <= input$daterange[2]) %>%
      group_by(Date) %>%
      summarise(Fouls = mean(HF + AF, na.rm = TRUE))
    
    ggplot(df_filtered, aes(x = Date, y = Fouls)) +
      geom_line(color = "#9932CC", size = 1) +
      geom_smooth(se = FALSE, color = "#FF69B4", linetype = "dashed") +
      labs(title = "Average Fouls per Match Over Time", x = "Date", y = "Fouls") +
      theme_minimal()
  })
}


shinyApp(ui, server)
