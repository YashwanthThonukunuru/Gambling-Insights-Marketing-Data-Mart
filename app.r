# Loading packages to R
library(haven)
require(haven)
library(anytime)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(data.table)
library(shiny)
library(dplyr)
library(plotly)
library(leaflet)
library(maps)
library(magrittr)
library(rvest)
library(DT)
library(shinydashboard)
library(ggplot2)
library(ggpubr)
library(scales)
library(stringr)

#https://rstudio.github.io/shinydashboard/get_started.html
#https://fontawesome.com/search?q=user&o=r&s=solid&f=sharp
#https://stackoverflow.com/questions/67641383/shinydashboard-sidebar-beginner-question-bulletpoints

setwd("C:/Users/ythonukunuru//OneDrive - IESEG/R Group Project/BWIN Dashboard")
# Import data to R
load("DataMart_BaseTable.RData")


#Create age group in 10s
#https://community.rstudio.com/t/dplyr-way-s-and-base-r-way-s-of-creating-age-group-from-age/89226/2
consolidated_datamart_df <- consolidated_datamart_df %>%
  mutate(age_group2 = case_when(
    Age > 10 & Age <= 19 ~"10-19",
    Age > 20 & Age <= 29 ~"20-29",
    Age > 30 & Age <= 39 ~"30-39",
    Age > 40 & Age <= 49 ~"40-49",
    Age > 50 & Age <= 59 ~"50-59",
    Age > 60 ~"60 & above"))

#Append all as filter options
gender_options <- c("All",sort(unique(consolidated_datamart_df$Gender)))
age_options <- c("All",sort(unique(consolidated_datamart_df$age_group2)))
language_options <- c("All",sort(unique(consolidated_datamart_df$Language)))
country_options <- c("All",sort(unique(consolidated_datamart_df$Country)))
application_options <- c("All",sort(unique(consolidated_datamart_df$Application)))

ui <- fluidPage(
  h1("Marketing Data Mart"),
  tabsetPanel(
    tabPanel(
      title = "User Profile",
      icon = icon("id-card"),
      fluidPage(
        fluidRow(
          column(2,selectInput("select_gender", "Select gender", gender_options)),
          column(2,selectInput("select_age", "Select age group", age_options)),
          column(2,selectInput("select_country", "Select country", country_options)),
          column(2,selectInput("select_language", "Select language", language_options)),
          column(2,selectInput("select_application", "Select application", application_options))
        ),
        fluidRow(
          box(plotlyOutput("gender",height = 250)),
          box(plotlyOutput("age_group",height = 250))
        ),
        h4("User Country Distribution"),
        leaflet::leafletOutput('map'),
        fluidRow(
          column(6, h4("Primary Language"),
                 DTOutput("language")),
          column(6, h4("User Application"),
                 DTOutput("application")))
      )),
    #tabPanel(title = "User Activity", icon = icon("user-gear")),
    tabPanel(
      title = "Betting", icon = icon("coins"),
      fluidPage(
        div(style="display:inline-block",selectInput("select_country", "Select country", country_options)),
        div(style="display:inline-block",selectInput("select_month", "Select month",
                                                   choices= c('All','February','March','April','June','July','August','September'))),
        div(style="display:inline-block",selectInput("select_product", "Select product",
                                                   choices = c('All','Sports book live-action','Poker BossMedia', 'Casino BossMedia', 'Supertoto', 'Games VS', 'Games bwin', 'Casino Chartwell'))),
        div(style="display:inline-block",selectInput("select_application", "Select application", application_options))),
        fluidRow(
          box(plotOutput("BetsPerProductPlot")),
          box(plotOutput("WinsPerProductPlot")),
          box(plotOutput("StakesPerProductPlot")),
          box(plotOutput("BetsTimeSeriesPlot")),
          box(plotOutput("WinsTimeSeriesPlot")),
          box(plotOutput("StakesTimeSeriesPlot")),
          box(plotOutput("CorelationScatterPlot")))),
    tabPanel(title = "Casino", icon = icon("dice"),
    fluidRow(
      column(3,
             radioButtons("graph", "Select Graph:",
                          choices = c("Total Poker Transactions per months" = 0 , "Average Amount of Poker Chips Bought Monthly" = 1, "Average Amount of Poker Chips Sold Monthly" = 2))
      ),
      column(9,
             plotOutput("plot",height = 500, width = 800)
      )
    )),
    
    tabPanel("Metadata",DT::dataTableOutput("data_table"), icon = icon("table")),
    
    ))

server <- function(input, output) {
  
  #Get the latitude and longtitude of each country to create an interactive map
  #https://www.kaggle.com/datasets/paultimothymooney/latitude-and-longitude-for-every-country-and-state?resource=download
  country_data <- read.csv("country_data.csv", stringsAsFactors=FALSE)
  colnames(country_data)[colnames(country_data) == "country"] <- "Country"
  consolidated_datamart_df <- left_join(consolidated_datamart_df, country_data, by='Country')

  
  #https://www.linkedin.com/pulse/shiny-app-r-integrating-filter-multiple-dynamic-conditions-lee-rock/
  #Create reactive functions for each filter 
  output_df <-consolidated_datamart_df
  
  #https://statisticsglobe.com/dplyr-message-summarise-has-grouped-output-r#example-1-reproduce-the-message-summarise-has-grouped-output-by-gr1-you-can-override-using-the-groups-argument
  options(dplyr.summarise.inform = FALSE)
  
  gender_data <- reactive({
    
    req(input$select_gender)
    req(input$select_age)
    req(input$select_language)
    req(input$select_country)
    req(input$select_application)
    
    if(input$select_gender == "All") {     
      filter1 <- quote(Gender != "@?><")         
    } else{      
      filter1 <- quote(Gender == input$select_gender)       
    }
    
    if(input$select_age == "All") {     
      filter_age <- quote(age_group2 != "@?><")         
    } else{      
      filter_age <- quote(age_group2 == input$select_age)       
    }
    
    if(input$select_language == "All") {     
      filter2 <- quote(Language != "@?><")         
    } else{      
      filter2 <- quote(Language == input$select_language)       
    }
    
    if(input$select_country== "All") {     
      filter3 <- quote(Country != "@?><")         
    } else{      
      filter3 <- quote(Country == input$select_country)       
    }
    
    if(input$select_application== "All") {     
      filter4 <- quote(Application != "@?><")         
    } else{      
      filter4 <- quote(Application == input$select_application)       
    }
    
    output_df %>%
      filter_(filter1) %>%
      filter_(filter_age) %>%
      filter_(filter2) %>%
      filter_(filter3) %>%
      filter_(filter4) %>%
      filter(!is.na(Gender)) %>%
      group_by(Gender) %>%
      summarize(count=n())
  })
  
  age_data <- reactive({
    
    req(input$select_gender)
    req(input$select_age)
    req(input$select_language)
    req(input$select_country)
    req(input$select_application)
    
    if(input$select_gender == "All") {     
      filter1 <- quote(Gender != "@?><")         
    } else{      
      filter1 <- quote(Gender == input$select_gender)       
    }
    
    if(input$select_age == "All") {     
      filter_age <- quote(age_group2 != "@?><")         
    } else{      
      filter_age <- quote(age_group2 == input$select_age)       
    }
    
    if(input$select_language == "All") {     
      filter2 <- quote(Language != "@?><")         
    } else{      
      filter2 <- quote(Language == input$select_language)       
    }
    
    if(input$select_country== "All") {     
      filter3 <- quote(Country != "@?><")         
    } else{      
      filter3 <- quote(Country == input$select_country)       
    }
    
    if(input$select_application== "All") {     
      filter4 <- quote(Application != "@?><")         
    } else{      
      filter4 <- quote(Application == input$select_application)       
    }
    
    output_df %>%
      filter_(filter1) %>%
      filter_(filter_age) %>%
      filter_(filter2) %>%
      filter_(filter3) %>%
      filter_(filter4) %>%
      filter(!is.na(age_group2)) %>%
      group_by(age_group2) %>%
      summarize(count=n()) %>%
      arrange(desc(count))
  })
  
  language_data <- reactive({
    
    req(input$select_gender)
    req(input$select_age)
    req(input$select_language)
    req(input$select_country)
    req(input$select_application)
    
    if(input$select_gender == "All") {     
      filter1 <- quote(Gender != "@?><")         
    } else{      
      filter1 <- quote(Gender == input$select_gender)       
    }
    
    if(input$select_age == "All") {     
      filter_age <- quote(age_group2 != "@?><")         
    } else{      
      filter_age <- quote(age_group2 == input$select_age)       
    }
    
    if(input$select_language == "All") {     
      filter2 <- quote(Language != "@?><")         
    } else{      
      filter2 <- quote(Language == input$select_language)       
    }
    
    if(input$select_country== "All") {     
      filter3 <- quote(Country != "@?><")         
    } else{      
      filter3 <- quote(Country == input$select_country)       
    }
    
    if(input$select_application== "All") {     
      filter4 <- quote(Application != "@?><")         
    } else{      
      filter4 <- quote(Application == input$select_application)       
    }
    
    output_df %>%
      filter_(filter1) %>%
      filter_(filter_age) %>%
      filter_(filter2) %>%
      filter_(filter3) %>%
      filter_(filter4) %>%
      group_by(Language) %>%
      summarize(count=n()) %>%
      arrange(desc(count))
  })  

  country_data <- reactive({
    
    req(input$select_gender)
    req(input$select_age)
    req(input$select_language)
    req(input$select_country)
    req(input$select_application)
    
    if(input$select_gender == "All") {     
      filter1 <- quote(Gender != "@?><")         
    } else{      
      filter1 <- quote(Gender == input$select_gender)       
    }
    
    if(input$select_age == "All") {     
      filter_age <- quote(age_group2 != "@?><")         
    } else{      
      filter_age <- quote(age_group2 == input$select_age)       
    }
    
    if(input$select_language == "All") {     
      filter2 <- quote(Language != "@?><")         
    } else{      
      filter2 <- quote(Language == input$select_language)       
    }
    
    if(input$select_country== "All") {     
      filter3 <- quote(Country != "@?><")         
    } else{      
      filter3 <- quote(Country == input$select_country)       
    }
    
    if(input$select_application== "All") {     
      filter4 <- quote(Application != "@?><")         
    } else{      
      filter4 <- quote(Application == input$select_application)       
    }
    
    output_df %>%
      filter_(filter1) %>%
      filter_(filter_age) %>%
      filter_(filter2) %>%
      filter_(filter3) %>%
      filter_(filter4) %>%
      filter(!is.na(latitude)) %>%
      group_by(Country) %>%
      summarize(count=n(), latitude, longitude)  %>%
      distinct(Country, count, latitude, longitude)
  })

  application_data <- reactive({
    
    req(input$select_gender)
    req(input$select_age)
    req(input$select_language)
    req(input$select_country)
    req(input$select_application)
    
    if(input$select_gender == "All") {     
      filter1 <- quote(Gender != "@?><")         
    } else{      
      filter1 <- quote(Gender == input$select_gender)       
    }
    
    if(input$select_age == "All") {     
      filter_age <- quote(age_group2 != "@?><")         
    } else{      
      filter_age <- quote(age_group2 == input$select_age)       
    }
    
    if(input$select_language == "All") {     
      filter2 <- quote(Language != "@?><")         
    } else{      
      filter2 <- quote(Language == input$select_language)       
    }
    
    if(input$select_country== "All") {     
      filter3 <- quote(Country != "@?><")         
    } else{      
      filter3 <- quote(Country == input$select_country)       
    }
    
    if(input$select_application== "All") {     
      filter4 <- quote(Application != "@?><")         
    } else{      
      filter4 <- quote(Application == input$select_application)       
    }
    
    output_df %>%
      filter_(filter1) %>%
      filter_(filter_age) %>%
      filter_(filter2) %>%
      filter_(filter3) %>%
      filter_(filter4) %>%
      group_by(Application) %>%
      summarize(count=n()) %>%
      arrange(desc(count))
  })    

  
  #https://community.plotly.com/t/incorporate-a-plotly-graph-into-a-shiny-app/5329/2
  #https://community.plotly.com/t/decimal-precision-in-pie-charts/31731/3
  output$gender <- renderPlotly({
    plot_ly(gender_data(), labels = ~Gender, values = ~count, type = 'pie',
            texttemplate = "%{percent:.0%}", marker = list(colors = c('#1E566C', '#ADD8E6'))) %>%
      layout(title = 'Gender Distribution', legend = list(x = 100, y = 0.5))
  })
  
  #https://statisticsglobe.com/order-bars-plotly-barchart-r#ascending-and-descending-values
  #https://plotly.com/r/bar-charts/
  output$age_group <- renderPlotly({
    plot_ly(age_data(), x = ~age_group2, y = ~count, type = 'bar', marker = list(color = ('#1E566C'))) %>%
      layout(yaxis =list(showgrid = FALSE, title = 'Number of Users'), title = 'Age Group Distribution') %>% 
      layout(xaxis = list(title = FALSE))
  })
  
  output$language <- renderDT({datatable(language_data(), filter="top")})
  
  #https://blogs.oracle.com/ai-and-datascience/post/a-beginners-exploration-of-shiny-and-leaflet-for-interactive-mapping
  output$map <- renderLeaflet({
    df <- country_data()
    
    map <- leaflet(data=df) %>% 
      addTiles(options = providerTileOptions(minZoom = 1.5, maxZoom = 5)) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       popup = paste(df$Country, ": ", df$count),
                       #radius = sqrt(country_data()$count) 
                       clusterOptions = markerClusterOptions())
    map
  })
  
  output$application <- renderDT({datatable(application_data(), filter="top")})

  
  
  #######################################
  #                                     #
  #       TOTAL BETS PER PRODUCT        #
  #                                     #
  #######################################
  
  # Preparing data to show total bets per product 
  product_perf_total_bets <- reactive({
    
    req(input$select_country)
    req(input$select_month)
    req(input$select_product)
    req(input$select_application)
    
    if(input$select_country== "All") {     
      filter5 <- quote(Country != "@?><")         
    } else{      
      filter5 <- quote(Country == input$select_country)       
    }
    
    if(input$select_month== "All") {     
      filter6 <- quote(Month != "@?><")         
    } else{      
      filter6 <- quote(Month == input$select_month)       
    }
    
    if(input$select_product== "All") {     
      filter7 <- quote(Product != "@?><")         
    } else{      
      filter7 <- quote(Product == input$select_product)       
    }
    
    if(input$select_application== "All") {     
      filter8 <- quote(Application != "@?><")         
    } else{      
      filter8 <- quote(Application == input$select_application)       
    }
    
    # Create subset of product wise metrics for total bets
    # https://datacornering.com/how-to-get-the-month-name-from-the-number-in-r/#:~:text=The%20easiest%20way%20to%20get,for%20the%20English%20month%20names.
    basetable_prod_selected <- consolidated_datamart_df %>%
      #filter_(filter5) %>%
      #filter_(filter6) %>%
      #filter_(filter7) %>%
      #filter_(filter8) %>%
      select(UserID,ends_with("_Total_bets"),FirstAct) %>%
      mutate(Month_name = format(FirstAct, "%B"))
    
    # Create subset of product wise metrics for total bets
    prod_selected_long <- basetable_prod_selected %>%  
      pivot_longer(cols = P2_Total_Bets:P8_Total_Bets,
                   names_to = "Product", 
                   values_to = "Total_bets")
    
    # Renaming values of 'Product' column in above data frame
    # https://stackoverflow.com/questions/66736264/how-do-i-rename-values-of-a-variable-in-r
    prod_selected_long <- prod_selected_long %>%
      mutate(Product = recode(Product, P2_Total_Bets = 'Sports book live-action',
                              P3_Total_Bets = 'Poker BossMedia',
                              P4_Total_Bets = "Casino BossMedia",
                              P5_Total_Bets = "Supertoto",
                              P6_Total_Bets = "Games VS",
                              P7_Total_Bets = "Games bwin",
                              P8_Total_Bets = "Casino Chartwell"))
    
  })    
  
  # Plot total bets per product
  output$BetsPerProductPlot <- renderPlot({
    ggplot(product_perf_total_bets(), aes(x = Product, y = Total_bets, fill = Product)) +
      geom_col() + 
      labs(title = "Total Bets per Product",
           x = "Product",
           y = "Total Bets") +
      theme(legend.position = "none",panel.background = element_rect(fill = "white")) +
      scale_y_continuous(labels = function(x) paste0(x / 1000000, "M"))
  })
  
  
  #######################################
  #                                     #
  #       TOTAL WINS PER PRODUCT        #
  #                                     #
  #######################################
  
  # Preparing data to show total wins per product 
  product_perf_total_wins <- reactive({
    
    req(input$select_country)
    req(input$select_month)
    req(input$select_product)
    req(input$select_application)
    
    if(input$select_country== "All") {     
      filter5 <- quote(Country != "@?><")         
    } else{      
      filter5 <- quote(Country == input$select_country)       
    }
    
    if(input$select_month== "All") {     
      filter6 <- quote(Month != "@?><")         
    } else{      
      filter6 <- quote(Month == input$select_month)       
    }
    
    if(input$select_product== "All") {     
      filter7 <- quote(Product != "@?><")         
    } else{      
      filter7 <- quote(Product == input$select_product)       
    }
    
    if(input$select_application== "All") {     
      filter8 <- quote(Application != "@?><")         
    } else{      
      filter8 <- quote(Application == input$select_application)       
    }
    
    # Create subset of product wise metrics for total wins
    basetable_prod_winnings_selected <- consolidated_datamart_df %>%
      select(UserID,ends_with("_Total_Winnings"),FirstAct,Betting_Net_Revenue,
             Country,Application) %>%
      #filter_(filter5) %>%
      #filter_(filter6) %>%
      #filter_(filter7) %>%
      filter_(filter8) %>%
      mutate(Month_name = format(FirstAct, "%B"))
    
    # Create subset of product wise metrics for total wins
    prod_selected_winnings_long <- basetable_prod_winnings_selected %>%  
      pivot_longer(cols = P2_Total_Winnings:P8_Total_Winnings,
                   names_to = "Product", 
                   values_to = "Total_Winnings")
    
    # Renaming values of 'Product' column in above data frame
    # https://stackoverflow.com/questions/66736264/how-do-i-rename-values-of-a-variable-in-r
    prod_selected_winnings_long <- prod_selected_winnings_long %>%
      mutate(Product = recode(Product, P2_Total_Winnings = 'Sports book live-action',
                              P3_Total_Winnings = 'Poker BossMedia',
                              P4_Total_Winnings = "Casino BossMedia",
                              P5_Total_Winnings = "Supertoto",
                              P6_Total_Winnings = "Games VS",
                              P7_Total_Winnings = "Games bwin",
                              P8_Total_Winnings = "Casino Chartwell"))
    
  })    
  
  # Plot total wins per product
  output$WinsPerProductPlot <- renderPlot({
    ggplot(product_perf_total_wins(), aes(x = Product, y = Total_Winnings, fill = Product)) +
      geom_col() + 
      labs(title = "Total Wins per Product",
           x = "Product",
           y = "Total Wins")  + 
      theme(legend.position = "none",panel.background = element_rect(fill = "white")) +
      scale_y_continuous(labels = function(x) paste0(x / 1000000, "M"))
  })
  
  
  #######################################
  #                                     #
  #       TOTAL STAKES PER PRODUCT      #
  #                                     #
  #######################################
  
  # Preparing data to show total stakes per product 
  product_perf_total_stakes <- reactive({
    
    req(input$select_country)
    req(input$select_month)
    req(input$select_product)
    req(input$select_application)
    
    if(input$select_country== "All") {     
      filter5 <- quote(Country != "@?><")         
    } else{      
      filter5 <- quote(Country == input$select_country)       
    }
    
    if(input$select_month== "All") {     
      filter6 <- quote(Month != "@?><")         
    } else{      
      filter6 <- quote(Month == input$select_month)       
    }
    
    if(input$select_product== "All") {     
      filter7 <- quote(Product != "@?><")         
    } else{      
      filter7 <- quote(Product == input$select_product)       
    }
    
    if(input$select_application== "All") {     
      filter8 <- quote(Application != "@?><")         
    } else{      
      filter8 <- quote(Application == input$select_application)       
    }
    
    # Create subset of product wise metrics for total stakes
    basetable_prod_stakes_selected <- consolidated_datamart_df %>%
      select(UserID,ends_with("_Total_Stakes"),FirstAct) %>%
      mutate(Month_name = format(FirstAct, "%B"))
    
    # Create subset of product wise metrics for total stakes
    prod_selected_stakes_long <- basetable_prod_stakes_selected %>%  
      pivot_longer(cols = P2_Total_Stakes:P8_Total_Stakes,
                   names_to = "Product", 
                   values_to = "Total_Stakes")
    
    # Renaming values of 'Product' column in above data frame
    # https://stackoverflow.com/questions/66736264/how-do-i-rename-values-of-a-variable-in-r
    prod_selected_stakes_long <- prod_selected_stakes_long %>%
      mutate(Product = recode(Product, P2_Total_Stakes = 'Sports book live-action',
                              P3_Total_Stakes = 'Poker BossMedia',
                              P4_Total_Stakes = "Casino BossMedia",
                              P5_Total_Stakes = "Supertoto",
                              P6_Total_Stakes = "Games VS",
                              P7_Total_Stakes = "Games bwin",
                              P8_Total_Stakes = "Casino Chartwell"))
    
  })    
  
  # Plot total stakes per product
  output$StakesPerProductPlot <- renderPlot({
    ggplot(product_perf_total_stakes(), aes(x = Product, y = Total_Stakes, fill = Product)) +
      geom_col() + 
      labs(title = "Total stakes per Product",
           x = "Product",
           y = "Total Stakes")  + 
      theme(legend.position = "none",panel.background = element_rect(fill = "white")) +
      scale_y_continuous(labels = function(x) paste0(x / 1000000, "M"))
  })
  
  ###############################################
  #                                             #
  #       TOTAL BETS OVER TIME PER PRODUCT      #
  #                                             #
  ###############################################
  
  # Create a line plot of total bets over time for each product
  output$BetsTimeSeriesPlot <- renderPlot({
    ggplot(product_perf_total_bets(), aes(x = Month_name, y = Total_bets,
                                   color = Product, fill = Product)) +
      geom_bar(stat = "identity") +
      labs(title = "Total bets since first active play date", x = "Month", y = "Total Bets") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.background = element_rect(fill = "white")) +
      scale_x_discrete(limits = c("February", "March", "April", "May", "June",
                                  "July", "August", "September")) +
      scale_y_continuous(labels = function(x) paste0(x / 1000000, "M"))
  })
  
  ###############################################
  #                                             #
  #       TOTAL WINS OVER TIME PER PRODUCT      #
  #                                             #
  ###############################################
  
  # Create a line plot of total wins over time for each product
  output$WinsTimeSeriesPlot <- renderPlot({
    ggplot(product_perf_total_wins(), aes(x = Month_name, y = Total_Winnings,
                                            color = Product, fill = Product)) +
      geom_bar(stat = "identity") +
      labs(title = "Total wins since first active play date", x = "Month", y = "Total Wins") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.background = element_rect(fill = "white")) +
      scale_x_discrete(limits = c("February", "March", "April", "May", "June",
                                  "July", "August", "September")) +
      scale_y_continuous(labels = function(x) paste0(x / 1000000, "M"))
  })
  
  ###############################################
  #                                             #
  #       TOTAL STAKES OVER TIME PER PRODUCT    #
  #                                             #
  ###############################################
  
  # Create a line plot of total stakes over time for each product
  output$StakesTimeSeriesPlot <- renderPlot({
    ggplot(product_perf_total_stakes(), aes(x = Month_name, y = Total_Stakes,
                                          color = Product, fill = Product)) +
      geom_bar(stat = "identity") +
      labs(title = "Total stakes since first active play date", x = "Month", y = "Total Wins") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.background = element_rect(fill = "white")) +
      scale_x_discrete(limits = c("February", "March", "April", "May", "June",
                                  "July", "August", "September")) +
      scale_y_continuous(labels = function(x) paste0(x / 1000000, "M"))
  })
  
  ############################################################
  #                                                          #
  #       CO-RELATION BETWEEN TOTAL WINS AND NET REVENUE     #
  #                                                          #
  ############################################################
  
  # Create a line plot of total stakes over time for each product
  output$CorelationScatterPlot <- renderPlot({
    ggplot(product_perf_total_wins(), aes(x = Total_Winnings, y = Betting_Net_Revenue, color = Product)) +
      geom_point(position = position_dodge(width = 0.2)) +
      geom_smooth(method = "lm") +
      scale_x_continuous(limits = c(0,600000),labels = function(x) paste0(x / 1000000, "M")) + 
      scale_y_continuous(limits = c(0, 45000),expand = c(0, 0.2)) +
      labs(title = "Total winnings vs bet revenue",
           x = "Total winnings",
           y = "Bet revenue") +
      theme(legend.position = "right",panel.background = element_rect(fill = "white"))
  })  
  
  ###############################
  
  data1 <- reactive({
    # Generate a data frame with columns x and y
    #fetch number of poker transactions columns from the datamart.
    nbr_pokerTrans_monthly <- consolidated_datamart_df %>%
      select(February_Nbr_PokerTrans:Oct_Nbr_PokerTrans)
    
    
    #replace NA values to 0
    index <- is.na(nbr_pokerTrans_monthly)
    nbr_pokerTrans_monthly[index] <- 0
    
    #finding the sum of number of transaction for each month.
    nbr_pokerTrans_monthly <- colSums(nbr_pokerTrans_monthly)
    nbr_pokerTrans_monthly <- data.frame(nbr_pokerTrans_monthly)
    
    #converting rownames to columns
    nbr_pokerTrans_monthly$months <- row.names(nbr_pokerTrans_monthly)
    
    #changing months
    nbr_pokerTrans_monthly$months<-str_replace_all(nbr_pokerTrans_monthly$months,"_Nbr_PokerTrans","")
    
    fortify(nbr_pokerTrans_monthly)
  })
  
  data2 <- reactive({
    ####Average number of Poker Chips purchased monthly ####
    avg_pokerBuy_monthly <- consolidated_datamart_df %>%
      select(February_AvgPokerBuy:Oct_AvgPokerBuy)
    
    #replace NA values to 0
    index <- is.na(avg_pokerBuy_monthly)
    avg_pokerBuy_monthly[index] <- 0
    
    #finding the sum of number of transaction for each month.
    avg_pokerBuy_monthly <- colSums(avg_pokerBuy_monthly)
    avg_pokerBuy_monthly <- data.frame(avg_pokerBuy_monthly)
    
    #converting rownames to columns
    avg_pokerBuy_monthly$months <- row.names(avg_pokerBuy_monthly)
    
    #changing months
    avg_pokerBuy_monthly$months<-str_replace_all(avg_pokerBuy_monthly$months,"_AvgPokerBuy","")
    
    fortify(avg_pokerBuy_monthly)
    
  })
  
  data3 <- reactive({
    #### Average number of Poker Chips Sold monthly ####
    avg_pokerSold_monthly <- consolidated_datamart_df %>%
      select(February_AvgPokerSell:Oct_AvgPokerSell)
    
    #replace NA values to 0
    index <- is.na(avg_pokerSold_monthly)
    avg_pokerSold_monthly[index] <- 0
    
    #finding the sum of number of transaction for each month.
    avg_pokerSold_monthly <- colSums(avg_pokerSold_monthly)
    avg_pokerSold_monthly <- data.frame(avg_pokerSold_monthly)
    
    #converting rownames to columns
    avg_pokerSold_monthly$months <- row.names(avg_pokerSold_monthly)
    
    #changing months
    avg_pokerSold_monthly$months<-str_replace_all(avg_pokerSold_monthly$months,"_AvgPokerSell","")
    
    fortify(avg_pokerSold_monthly)
  })
  
  output$plot <- renderPlot({
    if (input$graph == 0) {
      ggplot(data1(), aes(x=months,y=nbr_pokerTrans_monthly)) + geom_bar(stat = "identity")+
        ggtitle("Number of Poker Chip Transactions per Month") +
        xlab("Months") +
        ylab("Total No of Poker Chips") +
        scale_x_discrete(limits = c("February", "March", "April", "May", "June",
                                    "July", "August", "Sept","Oct"))
    } else if (input$graph == 1) {
      ggplot(data2(), aes(x = "", y = avg_pokerBuy_monthly, fill = months)) +
        geom_bar(stat = "identity",) +
        coord_polar("y", start = 0)+
        labs(title = "Average Amount of Poker Chip Purchase per Month")+
        theme(axis.title.x = element_blank())
    } else {
      ggplot(data3(), aes(x = months, y = avg_pokerSold_monthly)) +
        geom_point(size = 4)+
        ggtitle("Average Amount of Poker Chip Purchase per Month ") +
        xlab("Months") +
        ylab("Average Poker Sell Amount")+
        scale_x_discrete(limits = c("February", "March", "April", "May", "June",
                                    "July", "August", "Sept","Oct"))
    }
  })
  
  # Create a reactive data table
  data_table <- reactive({
    DT::datatable(consolidated_datamart_df, options = list(
      searching = TRUE,
      paging = TRUE
    ))
  })
    
  # Render the data table in the UI
  output$data_table <- DT::renderDataTable({
    data_table()
  })
  
}

  

# Run the application 
shinyApp(ui = ui, server = server)

