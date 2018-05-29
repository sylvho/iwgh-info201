library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

underage_prison_data <- read.csv('data/QT_less than 18 year olds_total.csv', 
                                 stringsAsFactors = FALSE)
female_prison_data <- read.csv('data/QT_less than 18 year olds_female.csv', 
                               stringsAsFactors = FALSE)
male_prison_data <- read.csv('data/QT_less than 18 year olds_male.csv', 
                             stringsAsFactors = FALSE)
colnames(underage_prison_data) <- c("Jurisdiction", "2000", "2001", "2002", "2003",
                                    "2004", "2005", "2006", "2007", "2008", "2009",
                                    "2010", "2011", "2012", "2013", "2014", "2015", "2016")
colnames(female_prison_data) <- c("Jurisdiction", "2000", "2001", "2002", "2003",
                                  "2004", "2005", "2006", "2007", "2008", "2009",
                                  "2010", "2011", "2012", "2013", "2014", "2015", "2016")
colnames(male_prison_data) <- c("Jurisdiction", "2000", "2001", "2002", "2003",
                                "2004", "2005", "2006", "2007", "2008", "2009",
                                "2010", "2011", "2012", "2013", "2014", "2015", "2016")
long_underage_data <- gather(underage_prison_data, key = year, value = value, "2000",
                             "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
                             "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
long_female_data <- gather(female_prison_data, key = year, value = Female, "2000",
                           "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
                           "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
long_male_data <- gather(male_prison_data, key = year, value = Male, "2000",
                         "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
                         "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")

long_underage_data$year <- as.numeric(long_underage_data$year)
long_underage_data$value <- as.numeric(long_underage_data$value)

long_female_data$year <- as.numeric(long_female_data$year)
long_female_data$Female <- as.numeric(long_female_data$Female)

long_male_data$year <- as.numeric(long_male_data$year)
long_male_data$Male <- as.numeric(long_male_data$Male)

long_underage_data <- left_join(long_underage_data, long_female_data)
long_underage_data <- left_join(long_underage_data, long_male_data)

year_range <- range(long_underage_data$year)

shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinythemes::shinytheme("superhero"),
      "US Prison Statistics",
      # First tab
      tabPanel("Prison Populations and Rates",
               sidebarPanel(
                #widget/sidebar here
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Tab 1", "This panel is intentionally left blank"),
                   tabPanel("Tab 2", "This panel is intentionally left blank"),
                   tabPanel("Tab 3", "This panel is intentionally left blank")
                 )
               )
      ),
      # Second tab
      tabPanel("Population and Overpopulation", 
               sidebarPanel(
                 #widget/sidebar here
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Tab 1", "This panel is intentionally left blank")
                 )
               )
      ),
      # Third tab
      tabPanel("Underage Inmate Population", 
               sidebarPanel(
                 selectInput('location_choice', label = "Select a location:", 
                             long_underage_data$Jurisdiction, selected = "Alabama", 
                             multiple = FALSE, selectize = TRUE),
                 sliderInput('year_choice', label="Select a range of years", min=year_range[1],
                             max=year_range[2], value=year_range, step = 1)
               ),
               mainPanel(
                 plotOutput('plot', click = 'plot_click'),
                 verbatimTextOutput("click_info"),
                 textOutput('analysis')
               )
      ),
      # Fourth tab 
      tabPanel("Non-Citizen Inmate Population", 
               sidebarPanel(
                 #widget/sidebar here
               ),
               mainPanel(
                 
               )
      )
    )
  ),
  
  server = function(input, output) {
    # For second tab: underage inmate population
    filtered_data <- reactive({
      data <- long_underage_data %>%
        filter(input$location_choice[1] == long_underage_data$Jurisdiction & 
                 long_underage_data$year >= input$year_choice[1] & 
                 long_underage_data$year <= input$year_choice[2])
      data2 <- rbind(
        data.frame("Year" = data$year, "Count" = data$Female, "Sex" = "Female"),
        data.frame("Year" = data$year, "Count" = data$Male, "Sex" = "Male")
      )
      return(data2)
    })
    
    output$plot <- renderPlot({
      ggplot(filtered_data(), aes(x = Year, y = Count, fill = Sex)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Underage Inmate Population in US (2010-2016)"
        )
    })
    
    output$click_info <- renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
    
    filtered_analysis <- reactive({
      analysis_filter <- long_underage_data %>%
        filter(input$location_choice[1] == long_underage_data$Jurisdiction & 
                 long_underage_data$year >= input$year_choice[1] & 
                 long_underage_data$year <= input$year_choice[2]) 
    })
    
    output$analysis <- renderText({
      return(paste0("The bar graph above shows the distribution of underage inmates
                  (inmates aged 17 years or younger) held in state prisons in ", 
                    input$location_choice[1], " from ", input$year_choice[1], " to ", 
                    input$year_choice[2], ". Overall, the total underage inmate population
                  in the US has gradually decreased over time, "))
    })
  }
)
