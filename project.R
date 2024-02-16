library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(qrcode)
library(lattice)
library(wordcloud2)
library(shiny)
library(leaflet)
library(maps)
library(sf)
library(shinydashboard)
library(lubridate)
library(viridis) # For color palettes
library(RColorBrewer)
library(scales) # For formatting axis text



## Building an R Shiny app


# Load the education data
education_data <- read_csv("database/Education.csv")
covid_data <- read_csv("database/us-states.csv")

# Aggregate the education data to state level
# 'State' is the state name or code that matches the shapefile
# and we are using "Percent of adults with less than a high school diploma"
education_state_level <- education_data %>%
  group_by(state) %>%
  summarise(
    Education_Less_HS = mean(`Percent of adults with less than a high school diploma`, na.rm = TRUE),
    High_School_Diploma_Only = mean(`Percent of adults with a high school diploma only`, na.rm = TRUE),
    Some_College = mean(`Percent of adults completing some college or associate's degree`, na.rm = TRUE),
    Bachelors_Or_Higher = mean(`Percent of adults with a bachelor's degree or higher`, na.rm = TRUE)
  )

# Aggregate the COVID data to state level
# Assuming 'state' is the state name or code that matches the shapefile
covid_state_level <- covid_data %>%
  group_by(state) %>%
  summarise(Cases = sum(cases), Deaths = sum(deaths))





# Create a Shiny app object
ui <- dashboardPage(
  dashboardHeader(title = "COVID and Education Level Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Graph", tabName = "line_graph", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
        fluidRow(
          column(
            width = 3,
            # Add a menu input for the user to select the education level
            selectInput("educationAttribute", "Choose Education Level:",
              choices = c(
                "Percent of adults with less than a high school diploma" = "Percent of adults with less than a high school diploma",
                "Percent of adults with a high school diploma only" = "High school diploma only",
                "Percent of adults with some college or associate's degree" = "Some college or associate's degree",
                "Percent of adults with a bachelor's degree or higher" = "Bachelor's degree or higher"
              )
            ),
            tags$div(style = "margin-bottom: 400px;"), # Adds 400px of space below the first input.
            selectInput("covidAttribute", "Choose COVID-19 Attribute:",
              choices = c(
                "Cases" = "Cases",
                "Deaths" = "Deaths",
                "Vaccine Distributed" = "doses_distributed",
                "Vaccine Administered" = "doses_administered"
              )
            )
          ),
          column(
            width = 9,
            tags$h3("Education Map"), # Adds the title above the first map
            leafletOutput("mapOutput"), # Display the education map
            tags$h3("COVID-19 Map"), # Adds the title above the second map
            leafletOutput("covidMapOutput") # Display the COVID map
          )
        )
      ),
      tabItem(
        tabName = "line_graph",
        fluidRow(
          column(
            width = 3,
            # Select the two attributes for the line graph
            selectInput("educationAttributeGraph", "Choose Education Level:", choices = c(
              "Percent of adults with less than a high school diploma" = "Percent of adults with less than a high school diploma",
              "Percent of adults with a high school diploma only" = "High school diploma only",
              "Percent of adults with some college or associate's degree" = "Some college or associate's degree",
              "Percent of adults with a bachelor's degree or higher" = "Bachelor's degree or higher"
            )),
            selectInput("covidAttributeGraph", "Choose COVID-19 Attribute:", choices = c(
              "Cases" = "Cases",
              "Deaths" = "Deaths",
              "Vaccine Distributed" = "doses_distributed",
              "Vaccine Administered" = "doses_administered"
            )),
            actionButton("plotGraph", "Plot Graph")
          ),
          column(
            width = 9,
            tags$p("The graph will display the relationship between the selected education level and COVID-19 attribute."),
            plotOutput("linePlot"),
            br(),
            tags$h3("Top 10 States by Doses Administered"),
            plotOutput("topTenStatesPlot"),
            br(),
            tags$h3("Vaccines Distributed vs. Administered by State Population"),
            plotOutput("bubbleChart")
          )
        )
      )
    )
  )
)


## Server function


# Step 1: Create a state name to abbreviation mapping

server <- function(input, output, session) {
  # Reactive expression for loading and preparing education data
  educationData <- reactive({
    education_data <- read_csv("database/Education.csv") %>%
      group_by(state) %>%
      summarise(
        Education_Level = mean(get(input$educationAttribute), na.rm = TRUE),
        Education_Less_HS = mean(`Percent of adults with less than a high school diploma`, na.rm = TRUE),
        High_School_Diploma_Only = mean(`Percent of adults with a high school diploma only`, na.rm = TRUE),
        Some_College = mean(`Percent of adults completing some college or associate's degree`, na.rm = TRUE),
        Bachelors_Or_Higher = mean(`Percent of adults with a bachelor's degree or higher`, na.rm = TRUE)
      )

    merged_data <- st_read("US_geographic_dataset/cb_2018_us_state_20m.shp") %>%
      st_transform(., 4326) %>%
      left_join(education_data, by = c("STUSPS" = "state"))

    return(merged_data)
  })

  education_data_reactive <- reactive({
    education_data <- read_csv("database/Education.csv") %>%
      select(State, `Percent of adults with a bachelor's degree or higher`) %>%
      arrange(desc(`Percent of adults with a bachelor's degree or higher`)) %>%
      head(10) # Keep top 10 states
    return(education_data)
  })

  # Reactive expression for loading and preparing COVID data for 2023-03-23
  covidData <- reactive({
    # Load COVID data
    covid_data <- read_csv("database/us-states-output.csv") %>%
      mutate(Date = as.Date(date, format = "%m/%d/%y")) %>%
      filter(Date == as.Date("2023-03-23", format = "%Y-%m-%d"))

    # Load and prepare vaccine data (assuming this step is correct)
    vaccine_data <- read_csv("database/cdc_vaccines_distributed_administered_by_jurisdiction.csv") %>%
      # Select the necessary columns for summarization
      select(state, doses_distributed, doses_administered)

    # Merge COVID data with vaccine data on state
    covid_data <- left_join(covid_data, vaccine_data, by = "state")

    # Summarise data after ensuring all necessary columns are present
    summarised_data <- covid_data %>%
      group_by(state) %>%
      summarise(
        COVID_Level = sum(.data[[input$covidAttribute]], na.rm = TRUE),
        Cases = sum(Cases, na.rm = TRUE),
        Deaths = sum(Deaths, na.rm = TRUE),
        Doses_Distributed = sum(doses_distributed, na.rm = TRUE),
        Doses_Administered = sum(doses_administered, na.rm = TRUE)
      )

    # Merge summarised data with geographic data
    merged_covid_data <- st_read("US_geographic_dataset/cb_2018_us_state_20m.shp") %>%
      st_transform(., 4326) %>%
      left_join(summarised_data, by = c("STUSPS" = "state"))

    return(merged_covid_data)
  })

  vaccine_data_reactive <- reactive({
    vaccine_data <- read_csv("database/cdc_vaccines_distributed_administered_by_jurisdiction.csv") %>%
      select(state, doses_administered_per_100k, population) %>%
      arrange(desc(doses_administered_per_100k)) %>%
      head(10) # Keep top 10 states with most doses administered per 100k people
    return(vaccine_data)
  })

    doses_death_data <- reactive({
    # Load vaccine administration data
    vaccine_data <- read_csv("database/cdc_vaccines_distributed_administered_by_jurisdiction.csv") %>%
      select(state, doses_administered)
    
    # Load death data
    death_data <- read_csv("database/us-states-output.csv") %>%
      select(state, Deaths)
    
    # Merge the datasets on the state column
    doses_death_data <- left_join(vaccine_data, death_data, by = "state")
    
    return(doses_death_data)
  })


  # Example reactive expression to fetch maximum values
  maxValues <- reactive({
    # Load the necessary data
    vaccine_data <- read_csv("database/cdc_vaccines_distributed_administered_by_jurisdiction.csv") %>%
      # Select the necessary columns for summarization
      select(state, doses_distributed, doses_administered)
    covid_data <- read_csv("database/us-states-output.csv")

    # Define maximum values for each attribute
    max_cases <- max(covid_data$Cases, na.rm = TRUE)
    max_deaths <- max(covid_data$Deaths, na.rm = TRUE)
    max_vaccines_distributed <- max(vaccine_data$doses_distributed, na.rm = TRUE)
    max_vaccines_administered <- max(vaccine_data$doses_administered, na.rm = TRUE)

    # Select the maximum value based on the current input selection
    if (input$covidAttribute == "Cases") {
      return(max_cases)
    } else if (input$covidAttribute == "Deaths") {
      return(max_deaths)
    } else if (input$covidAttribute == "doses_distributed") {
      return(max_vaccines_distributed)
    } else if (input$covidAttribute == "doses_administered") {
      return(max_vaccines_administered)
    } else {
      return(12000000) # Default case, should ideally never be reached
    }
  })


  pal1 <- colorBin(palette = "PuBuGn", domain = c(0, 30), bins = 5, na.color = "transparent")

  # Output for the map, which depends on the reactive education data
  output$mapOutput <- renderLeaflet({
    # Call the reactive expression and store its result
    data <- educationData() # Correctly calling the reactive expression

    leaflet(data = data) %>%
      fitBounds(-125, 24.396308, -66.934570, 49.384358) %>% # Fit the map to the US bounds
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~ colorQuantile("PuBuGn", Education_Level)(Education_Level),
        fillOpacity = 0.7,
        weight = 1,
        color = "white",
        dashArray = "3",
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
      ) %>%
      addLegend("bottomright",
        pal = pal1, values = c(0, 30), # Manually set legend values
        title = input$educationAttribute,
        opacity = 0.7,
        labFormat = labelFormat(suffix = "%")
      )
  })


  output$covidMapOutput <- renderLeaflet({
    data <- covidData() # Use the reactive COVID data
    # Generate popup content dynamically based on selected attribute YlOrRd PuBuGn
    maxValue <- maxValues() # This calls the reactive expression and gets its current value

    # Now create the color palette with the dynamically determined domain
    pal2 <- colorBin(palette = "YlOrRd", domain = c(0, maxValue), bins = 5, na.color = "transparent")


    leaflet(data = data) %>%
      fitBounds(-125, 24.396308, -66.934570, 49.384358) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~ colorQuantile("YlOrRd", COVID_Level)(COVID_Level),
        fillOpacity = 0.7,
        weight = 1,
        color = "white",
        dashArray = "3",
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = ~ paste(
          NAME, "<br>",
          "Cases: ", formatC(as.integer(Cases), big.mark = ","), "<br>",
          "Deaths: ", formatC(as.integer(Deaths), big.mark = ","), "<br>",
          "Doses Distributed: ", formatC(as.integer(Doses_Distributed), big.mark = ","), "<br>",
          "Doses Administered: ", formatC(as.integer(Doses_Administered), big.mark = ",")
        )
      ) %>%
      addLegend("bottomright",
        pal = pal2,
        values = c(0, maxValues()), # Use the reactive maximum value
        title = input$covidAttributes,
        opacity = 0.7,
        # labFormat = labelFormat(suffix = "%")
      )
  })

  observeEvent(input$plotGraph, {
    # Load COVID data
    covid_data <- read_csv("database/us-states-output.csv") %>%
      mutate(Date = as.Date(date, format = "%m/%d/%y")) %>%
      filter(Date == as.Date("2023-03-23", format = "%Y-%m-%d"))

    # Load and prepare vaccine data
    vaccine_data <- read_csv("database/cdc_vaccines_distributed_administered_by_jurisdiction.csv") %>%
      select(state, doses_distributed, doses_administered)

    # Merge COVID data with vaccine data on state
    merged_covid_vaccine_data <- left_join(covid_data, vaccine_data, by = "state")

    # Prepare the merged data based on user selection
    # Dynamically select the COVID attribute or vaccine data for summarization
    summarised_data <- merged_covid_vaccine_data %>%
      group_by(state) %>%
      summarise(COVID_Attribute = sum(case_when(
        input$covidAttributeGraph == "Cases" ~ Cases,
        input$covidAttributeGraph == "Deaths" ~ Deaths,
        input$covidAttributeGraph == "doses_distributed" ~ doses_distributed,
        input$covidAttributeGraph == "doses_administered" ~ doses_administered,
        TRUE ~ NA_real_ # Default case if none of the above match
      ), na.rm = TRUE)) %>%
      ungroup()

    # Load and prepare education data similarly to previous examples

    education_data <- read_csv("database/Education.csv") %>%
      group_by(state) %>%
      summarise(Education_Level = mean(get(input$educationAttributeGraph), na.rm = TRUE)) %>%
      ungroup()

    # Assuming 'State' in education_data matches 'state' in summarised_data
    # Merge education data with summarised COVID/vaccine data
    plot_data <- left_join(education_data, summarised_data, by = c("state" = "state"))

    # Generate the plot if plot_data is not empty
    if (nrow(plot_data) > 0) {
      output$linePlot <- renderPlot({
        ggplot(plot_data, aes(x = Education_Level, y = COVID_Attribute)) +
          geom_point() + # Choosing geom_point for a scatter plot
          geom_smooth(method = "lm", se = FALSE, color = "red") +
          labs(x = input$educationAttributeGraph, y = input$covidAttributeGraph, title = "Education Level vs. COVID-19 Attribute") +
          theme_minimal()
      })
    } else {
      output$linePlot <- renderPlot({
        ggplot() +
          geom_blank() +
          labs(title = "No data available for the selected criteria")
      })
    }
  })

  output$topTenStatesPlot <- renderPlot({
    vaccine_data <- vaccine_data_reactive() # Get the reactive vaccine data

    ggplot(vaccine_data, aes(x = reorder(state, doses_administered_per_100k), y = doses_administered_per_100k, fill = state)) +
      geom_col() + # Use geom_col for pre-summarized data
      coord_flip() + # Make the bar plot horizontal for better readability
      labs(x = "State", y = "Doses Administered per 100,000", title = "Top 10 States by Doses Administered per 100k") +
      theme_minimal() +
      scale_fill_viridis_d() + # Apply a discrete viridis color palette
      scale_y_continuous(labels = label_comma()) # Format y-axis labels to be more readable
  })
output$bubbleChart <- renderPlot({
  # Load vaccine data
  vaccine_data <- read_csv("database/cdc_vaccines_distributed_administered_by_jurisdiction.csv") %>%
    select(state, doses_distributed, doses_administered, population)
  
  # Load death data
  death_data <- read_csv("database/us-states-output.csv") %>%
    select(state, Deaths)
  
  # Merge the datasets on the state column
  merged_data <- left_join(vaccine_data, death_data, by = "state")
  
  # Create a bubble plot with ggplot
  ggplot(merged_data, aes(x = doses_distributed, y = doses_administered, 
                          size = population, color = Deaths)) +
    geom_point(alpha = 0.6) +
    scale_size(range = c(1, 20)) +
    scale_color_viridis_c() +
    labs(title = "Vaccines Distributed vs. Administered by State Population",
         x = "Doses Distributed",
         y = "Doses Administered",
         size = "Population",
         color = "Deaths") +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_x_continuous(labels = label_comma()) +  # Format x axis labels
    scale_y_continuous(labels = label_comma())    # Format y axis labels
})




  # No need for observeEvent if the map automatically updates with input$apply due to reactive dependencies
}

# Run the app
shinyApp(ui, server)
