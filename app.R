library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
# install.packages("shinythemes")

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

# Matt's data for Tab 1 #

prison_total <- read.csv("data/final_imprison_total_csv.csv",
                         stringsAsFactors = FALSE
)

prison_rate <- read.csv("data/final_imprison_rate_csv.csv",
                        stringsAsFactors = FALSE
)

colnames(prison_total) <- c("Jurisdiction", 1978:2016)

colnames(prison_rate) <- c("Jurisdiction", 1978:2016)


prison_total_long <- gather(prison_total,
                            key = year, value = pop, "1978", "1979", "1980", "1981", "1982", "1983",
                            "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992",
                            "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
                            "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                            "2010", "2011", "2012", "2013", "2014", "2015", "2016"
)

prison_total_long$pop <- as.numeric(prison_total_long$pop)

prison_total_long$pop <- as.numeric(prison_total_long$pop)

prison_rate_long <- gather(prison_rate,
                           key = year, value = pop, "1978", "1979", "1980", "1981", "1982", "1983",
                           "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992",
                           "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
                           "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                           "2010", "2011", "2012", "2013", "2014", "2015", "2016"
)

prison_rate_long$pop <- as.numeric(prison_rate_long$pop)

prison_rate_national <- filter(prison_rate_long, Jurisdiction == "U.S. total")

prison_rate_non_national <- filter(
  prison_rate_long,
  Jurisdiction != "Federal" & Jurisdiction != "State" &
    Jurisdiction != "U.S. total"
)

prison_total_point <- ggplot(data = prison_total_long) +
  geom_point(aes(x = year, y = pop, color = Jurisdiction))

prison_rate_point <- ggplot(data = prison_rate_long) +
  geom_point(aes(x = year, y = pop, color = Jurisdiction))

prison_rate_national_point <- ggplot(data = prison_rate_national) +
  geom_point(aes(x = year, y = pop, color = Jurisdiction))

prison_rate_non_national_point <- ggplot(data = prison_rate_non_national) +
  geom_point(aes(x = year, y = pop, color = Jurisdiction))

prison_total_national <- filter(
  prison_total_long,
  Jurisdiction == "U.S. total"
)

prison_total_national_point <- ggplot(data = prison_total_national) +
  geom_point(aes(x = year, y = pop, color = Jurisdiction))

state_map <- map_data("state")

prison_total_non_national <- filter(
  prison_total_long,
  Jurisdiction != "Federal" &
    Jurisdiction != "State"
)

prison_states_only <- filter(prison_total_long, Jurisdiction != "Federal"
                             & Jurisdiction != "State" & Jurisdiction != "U.S. total")

prison_states_only_lowercase <-
  sapply(prison_states_only$Jurisdiction, tolower)

prison_total_states_only <- mutate(prison_states_only,
                                   region = prison_states_only_lowercase
)

prison_state_map <- left_join(prison_total_states_only,
                              state_map,
                              by = "region"
)

prison_rate_non_national_lowercase <-
  sapply(prison_rate_non_national$Jurisdiction, tolower)

prison_rate_non_national <- mutate(prison_rate_non_national,
                                   region = prison_rate_non_national_lowercase
)

prison_rate_map <- left_join(prison_rate_non_national,
                             state_map,
                             by = "region"
)

###


shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinythemes::shinytheme("superhero"),
      "US Prison Statistics",
      # First tab
      tabPanel("Prison Populations and Rates",
               sidebarPanel(
                 selectInput("button",
                             label = "Years to Examine:",
                             choices = c(1978:2016)
                 ),
                 
                 checkboxGroupInput("checks",
                                    label = "Jurisdiction",
                                    choices =
                                      sort(unique(prison_total_non_national$Jurisdiction)),
                                    selected = "Alabama"
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Prison Populations by State", h4("Prison Populations by State, 1978-2016"), plotOutput("map1"),
                            plotlyOutput("chart1"),
                            plotlyOutput("chart3")),
                   tabPanel("Prison Rates by State", h4("Prison Rates by State. 1978-2016"), plotOutput("map2"),
                            plotlyOutput("chart2"),
                            plotlyOutput("chart4")),
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
                             long_underage_data$Jurisdiction, selected = "U.S. total", 
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
    
    # Matt's server info for Tab 1 #
    
    output$map1 <- renderPlot({
      prison_total_filter_map <- filter(prison_state_map, year == input$button)
      
      prison_map_total_year <- ggplot(data = prison_total_filter_map) +
        geom_polygon(aes(
          x = long, y = lat,
          group = group, fill = pop
        )) + scale_fill_continuous(low = "pink", high = "red") + theme_bw() +
        theme(axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
      
      return(prison_map_total_year)
    })
    
    output$map2 <- renderPlot({
      prison_rate_filter_map <- filter(prison_rate_map, year == input$button)
      
      prison_map_rate_year <- ggplot(data = prison_rate_filter_map) +
        geom_polygon(aes(
          x = long, y = lat,
          group = group, fill = pop
        )) + scale_fill_continuous(low = "grey", high = "blue") + theme_bw() +
        theme(axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
      
      return(prison_map_rate_year)
    })
    
    output$chart1 <- renderPlotly({
      prison_total_non_national_filter <- filter(
        prison_total_non_national,
        Jurisdiction %in% input$checks
      )
      
      prison_total_national_point <- ggplotly(ggplot(
        data = prison_total_non_national_filter
      ) +
        geom_point(aes(x = year, y = pop, color = Jurisdiction)) + labs(
          title = "Prison Population, 1978-2016",
          x = "Year",
          y = "Prison Population",
          color = "Jurisdiction"
        ))
      
      return(prison_total_national_point)
    })
    
    output$chart2 <- renderPlotly({
      prison_rate_national_filter <-
        filter(prison_rate_long, Jurisdiction %in% input$checks)
      
      prison_rate_national_point <- ggplotly(
        ggplot(data = prison_rate_national_filter) +
          geom_point(aes(x = year, y = pop, color = Jurisdiction)) + labs(
            title = "Prison Rate, 1978-2016",
            x = "Year",
            y = "Prisoners per 100,000 State Residents",
            color = "Jurisdiction"
          )
      )
      
      return(prison_rate_national_point)
    })
    
    output$chart3 <- renderPlotly({
      prison_total_national_point <- ggplotly(ggplot(data = prison_total_national) +
                                                geom_point(aes(x = year, y = pop)) + labs(
                                                  title = "National Prison Population, 1978-2016",
                                                  x = "Year",
                                                  y = "Prison Population"
                                                ))
      
      return(prison_total_national_point)
    })
    
    output$chart4 <- renderPlotly({
      prison_rate_national_point <- ggplotly(ggplot(data = prison_rate_national) +
                                               geom_point(aes(x = year, y = pop)) + labs(
                                                 title = "National Prison Rate, 1978-2016",
                                                 x = "Year",
                                                 y = "Prison Population"
                                               ))
      
      return(prison_rate_national_point)
    })
    
    ###
    
  }
)
