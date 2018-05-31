library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(readxl)
library(RColorBrewer)
library(tools)

#install.packages("plotly")
#install.packages("shinythemes")
#install.packages("readxl")

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

expect_rate <- c(
  773919, 874803, 942798, 951100, 959088, 966266,
  973505, 980346, 987659, 994892, 1001809
)

expect_years <- c(
  1990, 2000, 2008, 2009, 2010, 2011,
  2012, 2013, 2014, 2015, 2016
)

expect_frame <- data.frame(expect_years, expect_rate)

expect_frame <- mutate(expect_frame, year = expect_years)

expect_frame$expect_rate <- as.numeric(expect_frame$expect_rate)

prison_total_national$year <- as.numeric(prison_total_national$year)

expect_frame$expect_rate <- as.numeric(expect_frame$expect_rate)

expect_vs_national <- full_join(expect_frame,
  prison_total_national,
  by = "year"
)

expect_vs_national$Jurisdiction[1:11] <- "Projected"

expect_vs_national$pop[1:11] <- expect_rate

rate_non_national_2000 <- filter(prison_rate_non_national, year == 2000) %>%
  arrange(-pop)

rate_non_national_2007 <- filter(prison_rate_non_national, year == 2007) %>%
  summarise(mean = mean(pop))

# Jevandre's data for tab 2

capacities <- read.csv("data/capacity_data.csv", stringsAsFactors = FALSE)
capacities <- capacities %>%
  mutate(
    rate.pct = 100 * capacities$Custody.population / capacities$Rate,
    operational.pct = 100 * capacities$Custody.population /
      capacities$Operational,
    design.pct = 100 * capacities$Custody.population / capacities$Design
  )
capacities$Jurisdiction <- tolower(capacities$Jurisdiction)
map_data <- left_join(state_map, capacities,
  by = c("region" = "Jurisdiction")
)

# Sylvia's data for Tab 3 #

underage_prison_data <- read.csv("data/QT_less than 18 year olds_total.csv",
                                 stringsAsFactors = FALSE
)
female_prison_data <- read.csv("data/QT_less than 18 year olds_female.csv",
                               stringsAsFactors = FALSE
)
male_prison_data <- read.csv("data/QT_less than 18 year olds_male.csv",
                             stringsAsFactors = FALSE
)

# Rename year columns 
colnames(underage_prison_data) <- c(
  "Jurisdiction", "2000", "2001", "2002", "2003",
  "2004", "2005", "2006", "2007", "2008", "2009",
  "2010", "2011", "2012", "2013", "2014", "2015", "2016"
)
colnames(female_prison_data) <- c(
  "Jurisdiction", "2000", "2001", "2002", "2003",
  "2004", "2005", "2006", "2007", "2008", "2009",
  "2010", "2011", "2012", "2013", "2014", "2015", "2016"
)
colnames(male_prison_data) <- c(
  "Jurisdiction", "2000", "2001", "2002", "2003",
  "2004", "2005", "2006", "2007", "2008", "2009",
  "2010", "2011", "2012", "2013", "2014", "2015", "2016"
)

# Convert into long data
long_underage_data <- gather(underage_prison_data,
                             key = year, value = value, "2000",
                             "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                             "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"
)
long_female_data <- gather(female_prison_data,
                           key = year, value = Female, "2000",
                           "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                           "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"
)
long_male_data <- gather(male_prison_data,
                         key = year, value = Male, "2000",
                         "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                         "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"
)

long_underage_data$year <- as.numeric(long_underage_data$year)
long_underage_data$value <- as.numeric(long_underage_data$value)

long_female_data$year <- as.numeric(long_female_data$year)
long_female_data$Female <- as.numeric(long_female_data$Female)

long_male_data$year <- as.numeric(long_male_data$year)
long_male_data$Male <- as.numeric(long_male_data$Male)

# Join all datasets into one
long_underage_data <- left_join(long_underage_data, long_female_data)
long_underage_data <- left_join(long_underage_data, long_male_data)

year_range <- range(long_underage_data$year)

# Rebecca's data for tab 4
data <- read_xlsx("data/QT_noncitizens_total.xlsx", sheet = 1)

###

shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinythemes::shinytheme("superhero"),
      "US Prison Statistics",
      # First tab
      tabPanel(
        "Prison Populations and Rates",
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
          ),
          p(
            "Data for the District of Columbia is only available up to 2000."
          )
        ),

        mainPanel(
          tabsetPanel(
            tabPanel(
              "Prison Populations by State",
              h4(
                "Prison Populations by State in ",
                textOutput("curr1", inline = T)
              ),
              plotOutput("map1"),
              plotlyOutput("chart1"),
              plotlyOutput("chart3")
            ),
            tabPanel(
              "Prison Rates by State",
              h4(
                "Prison Rates by State in ",
                textOutput("curr2", inline = T)
              ),
              plotOutput("map2"),
              plotlyOutput("chart2"),
              plotlyOutput("chart4")
            ),
            tabPanel("Analysis", "This panel is intentionally left blank")
          )
        )
      ),
      # Second tab
      tabPanel(
        "Population and Overpopulation",
        sidebarPanel(
          sliderInput("year_choice_j", label = "Year", min = 2011, max = 2016, value = 2011, step = 1),
          selectInput("option_choice",
            label = "Stats",
            choices = list(
              "Custody Population" = "Custody.population",
              "Rate %" = "rate.pct",
              "Operational %" = "operational.pct",
              "Design %" = "design.pct"
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Tab 1", plotOutput("map"))
          )
        )
      ),
      # Third tab
      tabPanel(
        "Underage Inmate Population",
        sidebarPanel(
          # Create dropdown menu of states
          selectInput("location_choice",
            label = "Select a location:",
            long_underage_data$Jurisdiction, selectize = FALSE
          ),
          # Create slider to select range of years
          sliderInput("year_choice",
            label = "Select a range of years", min = year_range[1],
            max = year_range[2], value = year_range, step = 1
          )
        ),
        mainPanel(
          tabsetPanel(
            # Tab for visaulization and description
            tabPanel("Plot", plotOutput("plot", click = "plot_click"), 
                     verbatimTextOutput("click_info"),
                     textOutput("analysis")),
            # Tab for further analysis
            tabPanel("Analysis", htmlOutput("further_analysis"))
          )
        )
      ),
      # Fourth tab
      tabPanel(
        "Non-Citizen Inmate Population",
        sidebarPanel(
          selectInput("year", "Year", data$year), 
          
          br(),
          
          radioButtons("viz", "Visualization", c("Bar Chart" = "bar", 
                                                 "Map Chart" = "map"))
        ),
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("U.S. Total", plotOutput("us_plot", height = 800)), 
                      tabPanel("Federal", plotOutput("federal_plot", height = 800)),
                      tabPanel("States", plotOutput("states_plot", height = 800)))
        )
      )
    )
  ),

  server = function(input, output) {

    # Matt's server info for Tab 1 #

    output$map1 <- renderPlot({
      prison_total_filter_map <- filter(prison_state_map, year == input$button)

      prison_map_total_year <- ggplot(data = prison_total_filter_map) +
        geom_polygon(aes(
          x = long, y = lat,
          group = group, fill = pop
        )) +
        scale_fill_continuous(low = "pink", high = "red") + theme_bw() +
        theme(
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
        )

      return(prison_map_total_year)
    },
    width = 700,
    height = 350
    )

    output$map2 <- renderPlot({
      prison_rate_filter_map <- filter(prison_rate_map, year == input$button)

      prison_map_rate_year <- ggplot(data = prison_rate_filter_map) +
        geom_polygon(aes(
          x = long, y = lat,
          group = group, fill = pop
        )) + scale_fill_continuous(low = "grey", high = "blue") + theme_bw() +
        theme(
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
        )

      return(prison_map_rate_year)
    },
    width = 700,
    height = 350
    )

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
        ) + theme(
          text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )) %>% layout(margin = list(l = 85))

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
          ) + theme(
            text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
      ) %>% layout(margin = list(l = 85))

      return(prison_rate_national_point)
    })

    output$chart3 <- renderPlotly({
      prison_total_national_point <- ggplotly(ggplot(data = prison_total_national) +
        geom_point(aes(x = year, y = pop)) +
        geom_smooth(data = expect_frame, aes(x = expect_years, y = expect_rate)) +
        labs(
          title = "National Prison Population, 1978-2016",
          x = "Year",
          y = "Prison Population"
        ) + theme(
          text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )) %>% layout(margin = list(l = 85))

      return(prison_total_national_point)
    })

    output$chart4 <- renderPlotly({
      prison_rate_national_point <- ggplotly(ggplot(data = prison_rate_national) +
        geom_point(aes(x = year, y = pop, color = Jurisdiction)) + labs(
          title = "National Prison Rate, 1978-2016",
          x = "Year",
          y = "Prison Population"
        ) + theme(
          text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )) %>% layout(margin = list(l = 85))

      return(prison_rate_national_point)
    })


    output$curr1 <- renderText({
      return(input$button)
    })

    output$curr2 <- renderText({
      return(input$button)
    })

    # Jevandre's server info for tab 2
    output$map <- renderPlot({
      map_data_year <- filter(map_data, Year == input$year_choice_j) %>%
        select(long, lat, group, choice = input$option_choice)
      map_plot <- ggplot(data = map_data_year) +
        geom_polygon(aes(
          x = long, y = lat,
          group = group, fill = choice
        )) +
        scale_fill_continuous(low = "yellow", high = "red") +
        theme_bw() +
        theme(
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
        )
      return(map_plot)
    },
    width = 700,
    height = 350
    )
    
    # Sylvia's server info for Tab 3
    
    # Filter data for visualization based on input
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
    
    # Create bar graph 
    output$plot <- renderPlot({
      ggplot(filtered_data(), aes(x = Year, y = Count, fill = Sex)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Underage Inmate Population in US (2010-2016)"
        )
    })
    
    # Create click user interaction
    output$click_info <- renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
    
    # Filter data for first year selected
    filtered_analysis_1 <- reactive({
      analysis_filter <- long_underage_data %>%
        filter(input$location_choice[1] == long_underage_data$Jurisdiction & 
                 long_underage_data$year == input$year_choice[1]) 
      return(analysis_filter$value)
    })
    
    # Filter data for last year selected
    filtered_analysis_2 <- reactive({
      analysis_filter <- long_underage_data %>%
        filter(input$location_choice[1] == long_underage_data$Jurisdiction & 
                 long_underage_data$year == input$year_choice[2]) 
      return(analysis_filter$value)
    })
    
    # Filter data to calculate mean based on state/year range selection
    filtered_analysis_mean <- reactive({
      analysis_filter <- long_underage_data %>%
        filter(input$location_choice[1] == long_underage_data$Jurisdiction & 
                 long_underage_data$year >= input$year_choice[1] &
                 long_underage_data$year <= input$year_choice[2]) %>%
        summarize(mean = mean(value))
      return(round(analysis_filter))
    })
    
    # Filter data for females
    filtered_analysis_female <- reactive({
      analysis_filter <- long_underage_data %>%
        filter(input$location_choice[1] == long_underage_data$Jurisdiction & 
                 long_underage_data$year >= input$year_choice[1] &
                 long_underage_data$year <= input$year_choice[2])
      sum <- sum(analysis_filter$Female)
      return(sum)
    })
    
    # Filter data for males
    filtered_analysis_male <- reactive({
      analysis_filter <- long_underage_data %>%
        filter(input$location_choice[1] == long_underage_data$Jurisdiction & 
                 long_underage_data$year >= input$year_choice[1] &
                 long_underage_data$year <= input$year_choice[2])
      sum <- sum(analysis_filter$Male)
      return(sum)
    })
    
    # Output text for visualiation description 
    output$analysis <- renderText({
      return(paste0("The bar graph above shows the distribution of underage inmates
                    (inmates aged 17 years or younger) held in state prisons in ", 
                    input$location_choice[1], " from ", input$year_choice[1], " to ", 
                    input$year_choice[2], ". Overall, the total underage inmate population
                    in ", input$location_choice[1]," has gradually decreased over time. 
                    The population went from ", filtered_analysis_1(), " inmates in ", 
                    input$year_choice[1], " to ", filtered_analysis_2(), " inmates in ",
                    input$year_choice[2], ". The mean number of inmates in ", 
                    input$location_choice[1], " for each year is ", filtered_analysis_mean(),
                    ". The ratio of the sexes of the underage inmate population is heavily
                    skewed towards males. From ", input$year_choice[1], " to ", 
                    input$year_choice[2], ", there was a total underage female inmate 
                    population of ", filtered_analysis_female(), 
                    " compared to the much larger total underage male inmate population of ", 
                    filtered_analysis_male(), " in ", input$location_choice[1], ". Data drawn from
                    Bureau of Justice Statistics."))
    })
    
    # Output text for further analysis
    output$further_analysis <- renderText({
      return(paste0("<B><I>How have the number of inmates in federal or state prisons under the 
                    age of 17 changed over time and what is the gender composition of the 
                    underage inmate population?</B></I> Between 2001 and 2015, the US juvenile 
                    inprisonment rates fell by 54%.
                    This decline is attributed mostly in part to state policymakers working 
                    to reform the juvenile incarceration system by looking to use alternate 
                    forms of punishment instead of incarceration. The drop in the underage 
                    inmate population is important because delinquency is frequently associated 
                    with higher crime rates and greater risk of physical and mental struggles 
                    in adulthood, as well as a lower likelihood of completing high school. 
                    Furthermore, underage inmates are more likely to be male than female, as 
                    males have consistently accounted for about 85% of the juvenile population.
                    Data drawn from Bureau of Justice Statistics."))
    })
    
    ### Rebecca's server info for tab 4
    states_plot <- reactive({
      summary <- filter(data, Jurisdiction == "U.S.total" | Jurisdiction == "Federal" | Jurisdiction == "State")
      states <- anti_join(data, summary) %>% 
        filter(states$year == input$year)
      states_coor_2 <- map_data("state")
      colnames(states_coor_2)[colnames(states_coor_2) == "region"] <- "Jurisdiction"
      states_coor_2$Jurisdiction <- toTitleCase(states_coor_2$Jurisdiction)
      states_coor <- left_join(states, states_coor_2, by = "Jurisdiction") %>% 
        select(long, lat, Jurisdiction, number, year)
      states_coor <- na.omit(states_coor)
      
      chart <- ggplot(data = states) + 
        geom_bar(mapping = aes(x = Jurisdiction, y = number, fill = Jurisdiction), position = "dodge", stat = "identity") + 
        scale_color_brewer(palette = "Set3") +
        coord_flip()
      
      state_map <- switch (input$viz,
                           bar = chart <- ggplot(data = states) + 
                             geom_bar(mapping = aes(x = Jurisdiction, y = number, fill = Jurisdiction), position = "dodge", stat = "identity") + 
                             scale_color_brewer(palette = "Set3") +
                             coord_flip(),
                           map = chart <- ggplot(data = states_coor) + 
                             geom_polygon(aes(x = long, y = lat, fill = number, group = Jurisdiction)) + 
                             scale_fill_gradientn(colours = brewer.pal(name = "Set3", n = 10))
      )
      
      chart
    })
    
    us_plot <- reactive({
      summary <- filter(data, Jurisdiction == "U.S.total")
      
      chart <- ggplot(data = summary) + 
        geom_bar(mapping = aes(x = year, y = number, fill = year), position = "dodge", stat = "identity") + 
        scale_color_brewer(palette = "Set3")
      
      chart
    })
    
    federal_plot <- reactive({
      summary <- filter(data, Jurisdiction == "Federal")
      
      chart <- ggplot(data = summary) + 
        geom_bar(mapping = aes(x = year, y = number, fill = year), position = "dodge", stat = "identity") + 
        scale_color_brewer(palette = "Set3")
      
      chart
    })
    
    output$states_plot <- renderPlot({states_plot()})
    output$us_plot <- renderPlot({us_plot()})
    output$federal_plot <- renderPlot({federal_plot()})
  }
)
