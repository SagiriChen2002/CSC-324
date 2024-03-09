# Import libraries
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
library(viridis)
library(RColorBrewer)
library(scales)



## Building an R Shiny app

# Load the education data
education_data <- read_csv("database/Education.csv")
# Load the COVID data
covid_data <- read_csv("database/us-states.csv")

# Aggregate the education data to state level
# Calculating the mean of each education level for each state
education_state_level <- education_data %>%
  group_by(state) %>%
  summarise(
    Education_Less_HS = mean(`Percent of adults with less than a high school diploma`, na.rm = TRUE),
    High_School_Diploma_Only = mean(`Percent of adults with a high school diploma only`, na.rm = TRUE),
    Some_College = mean(`Percent of adults completing some college or associate's degree`, na.rm = TRUE),
    Bachelors_Or_Higher = mean(`Percent of adults with a bachelor's degree or higher`, na.rm = TRUE)
  )

# Aggregate the COVID data to state level
covid_state_level <- covid_data %>%
  group_by(state) %>%
  summarise(Cases = sum(cases), Deaths = sum(deaths))





# Define the UI for the Shiny app
ui <- dashboardPage(
  # Define the dashboard header and name of the app
  dashboardHeader(title = "COVID and Education Level Analysis"),
  # Define the dashboard sidebar with menu items
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Graph", tabName = "line_graph", icon = icon("line-chart")),
      menuItem("Description", tabName = "description", icon = icon("info-circle"))
    )
  ),
  # Define the dashboard body with tab items
  # Each tab item will contain the content for the respective tab
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
        fluidRow(
          column(
            width = 3,
            # Select the first attributes for the map (Education Level)
            selectInput("educationAttribute", "Choose Education Level:",
              choices = c(
                "Percent of adults with less than a high school diploma" = "Percent of adults with less than a high school diploma",
                "Percent of adults with a high school diploma only" = "Percent of adults with a high school diploma only",
                "Percent of adults with some college or associate's degree" = "Percent of adults completing some college or associate's degree",
                "Percent of adults with a bachelor's degree or higher" = "Percent of adults with a bachelor's degree or higher"
              )
            ),
            # Adds 400px of space below the first input to make space for the second inputs
            tags$div(style = "margin-bottom: 400px;"),
            # Select the two attributes for the map (COVID-19 data)
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
            # Adds the title above the first map
            tags$h3("Education Map"),
            # Display the education map
            leafletOutput("mapOutput"),
            # Adds the title above the second map
            tags$h3("COVID-19 Map"),
            # Display the COVID map
            leafletOutput("covidMapOutput"),
            
          )
        )
      ),
      # Define the second tab item for the line graph (the line graph)
      tabItem(
        tabName = "line_graph",
        fluidRow(
          column(
            width = 3,
            # Select the education attribute for the graph
            selectInput("educationAttributeGraph", "Choose Education Level:", choices = c(
              "Percent of adults with less than a high school diploma" = "Percent of adults with less than a high school diploma",
              "Percent of adults with a high school diploma only" = "Percent of adults with a high school diploma only",
              "Percent of adults with some college or associate's degree" = "Percent of adults completing some college or associate's degree",
              "Percent of adults with a bachelor's degree or higher" = "Percent of adults with a bachelor's degree or higher"
            )),
            # Select the COVID-19 attribute for the graph
            selectInput("covidAttributeGraph", "Choose COVID-19 Attribute:", choices = c(
              "Cases" = "Cases",
              "Deaths" = "Deaths",
              "Vaccine Distributed" = "Vaccine Distributed",
              "Vaccine Administered" = "Vaccine Administered"
            )),
            # Add a button to plot the graph
            actionButton("plotGraph", "Plot Graph")
          ),
          # Display the line plot, top 10 states plot, and bubble chart about vaccine distribution and death
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
      ),
      # Define the third tab item for the description
      tabItem(
        tabName = "description",
        fluidRow(
          # Display the description and explanation of the project
          column(
            width = 9,
            textOutput("text"),
            tableOutput("description")
          )
        )
      )
    )
  )
)


## Server function

# Define the server function
server <- function(input, output, session) {
  # Reactive function for loading and preparing education data
  educationData <- reactive({
    # Load education data
    education_data <- read_csv("database/Education.csv") %>%
      # Select the necessary columns for summarization
      group_by(state) %>%
      # Calculate the mean of each education level for each state
      summarise(
        Education_Level = mean(get(input$educationAttribute), na.rm = TRUE),
        Education_Less_HS = mean(`Percent of adults with less than a high school diploma`, na.rm = TRUE),
        High_School_Diploma_Only = mean(`Percent of adults with a high school diploma only`, na.rm = TRUE),
        Some_College = mean(`Percent of adults completing some college or associate's degree`, na.rm = TRUE),
        Bachelors_Or_Higher = mean(`Percent of adults with a bachelor's degree or higher`, na.rm = TRUE)
      )

    # Merge education data with geographic data to generate the map
    merged_data <- st_read("US_geographic_dataset/cb_2018_us_state_20m.shp") %>%
      # Transform the data to the correct coordinate system
      st_transform(., 4326) %>%
      left_join(education_data, by = c("STUSPS" = "state"))

    return(merged_data)
  })

  # Reactive function for loading and preparing education with top 10 states data
  education_data_reactive <- reactive({
    # Load education data
    education_data <- read_csv("database/Education.csv") %>%
      # Select the necessary columns for summarizations
      select(State, `Percent of adults with a bachelor's degree or higher`) %>%
      # Arrange the data in descending order
      arrange(desc(`Percent of adults with a bachelor's degree or higher`)) %>%
      # Keep top 10 states
      head(10)
    return(education_data)
  })

  # Reactive expression for loading and preparing COVID data
  covidData <- reactive({
    # Load COVID data
    covid_data <- read_csv("database/us-states-output.csv") %>%
      # Format the date column and filter
      mutate(Date = as.Date(date, format = "%m/%d/%y")) %>%
      filter(Date == as.Date("2023-03-23", format = "%Y-%m-%d"))

    # Load vaccine data
    vaccine_data <- read_csv("database/cdc_vaccines_distributed_administered_by_jurisdiction.csv") %>%
      # Select state, doses distributed, and doses administered
      select(state, doses_distributed, doses_administered)

    # Merge COVID data with vaccine data on state
    covid_data <- left_join(covid_data, vaccine_data, by = "state")
    # Summarise data after ensuring all necessary columns are present
    summarised_data <- covid_data %>%
      group_by(state) %>%
      # Summarise the data based on the user's input
      summarise(
        COVID_Level = sum(.data[[input$covidAttribute]], na.rm = TRUE),
        Cases = sum(Cases, na.rm = TRUE),
        Deaths = sum(Deaths, na.rm = TRUE),
        Doses_Distributed = sum(doses_distributed, na.rm = TRUE),
        Doses_Administered = sum(doses_administered, na.rm = TRUE)
      )

    # Merge summarised data with geographic data
    merged_covid_data <- st_read("US_geographic_dataset/cb_2018_us_state_20m.shp") %>%
      # Transform the data to the correct coordinate system
      st_transform(., 4326) %>%
      left_join(summarised_data, by = c("STUSPS" = "state"))

    return(merged_covid_data)
  })

  # Reactive function for loading and preparing vaccine data
  vaccine_data_reactive <- reactive({
    # Load vaccine data
    vaccine_data <- read_csv("database/cdc_vaccines_distributed_administered_by_jurisdiction.csv") %>%
      # Select the state, doses administered, and population columns
      select(state, doses_administered_per_100k, population) %>%
      arrange(desc(doses_administered_per_100k)) %>%
      # Keep top 10 states with most doses administered per 100k people
      head(10)
    return(vaccine_data)
  })

  # Reactive function for loading and preparing vaccine and death data
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


  # Reactive function for determining maximum values for the COVID map for legend
  maxValues <- reactive({
    # Load vaccine data
    vaccine_data <- read_csv("database/cdc_vaccines_distributed_administered_by_jurisdiction.csv") %>%
      # Select the necessary columns for summarization
      select(state, doses_distributed, doses_administered)
    # Load COVID data
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
      # Set default value to 12000000 (never reached in this case)
      return(12000000)
    }
  })


  # Create a color palette for the education map
  pal1 <- colorBin(palette = "PuBuGn", domain = c(0, 30), bins = 5, na.color = "transparent")

  # Output for the map, which depends on the reactive education data
  output$mapOutput <- renderLeaflet({
    # Call the reactive expression and store its result
    data <- educationData()
    # Generate the map with leaflet
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
      # Add a legend to the map
      addLegend("bottomright",
        pal = pal1, values = c(0, 30),
        title = input$educationAttribute,
        opacity = 0.7,
        labFormat = labelFormat(suffix = "%")
      )
  })


  # Output for the COVID map, which depends on the reactive COVID data
  output$covidMapOutput <- renderLeaflet({
    #  Use the reactive COVID data
    data <- covidData()
    # Get the maximum value for the color palette
    maxValue <- maxValues()

    # Create a color palette for the COVID map
    pal2 <- colorBin(palette = "YlOrRd", domain = c(0, maxValue), bins = 5, na.color = "transparent")

    # Generate the map with leaflet
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
        # Add a popup with the state name and COVID-19 information
        popup = ~ paste(
          NAME, "<br>",
          "Cases: ", formatC(as.integer(Cases), big.mark = ","), "<br>",
          "Deaths: ", formatC(as.integer(Deaths), big.mark = ","), "<br>",
          "Doses Distributed: ", formatC(as.integer(Doses_Distributed), big.mark = ","), "<br>",
          "Doses Administered: ", formatC(as.integer(Doses_Administered), big.mark = ",")
        )
      ) %>%
      # Add a legend to the map
      addLegend("bottomright",
        pal = pal2,
        values = c(0, maxValues()),
        title = input$covidAttributes,
        opacity = 0.7,
      )
  })

  # Output for the description
  output$text <- renderText({
    paste("In this project, we aim to analyze the relationship between the education level and COVID-19 information, including cases, death, and vaccines.\n",
      "The first tab contains two maps. The first map displays the education level of each state, and the second map displays the COVID-19 attribute of each state.\n",
      "The second tab contains a line graph, a bar plot, and a bubble chart. The line graph displays the relationship between the selected education level and COVID-19 attribute.",
      sep = ""
    )
  })

  # Output for the description table
  output$description <- renderTable({
    data.frame(
      Description = c("Percent of adults with less than a high school diploma",
                      "Percent of adults with a high school diploma only",
                      "Percent of adults completing some college or associate's degree",
                      "Percent of adults with a bachelor's degree or higher",
                      "Cases",
                      "Deaths",
                      "Vaccine Distributed",
                      "Vaccine Administered"),
      Explanation = c("The percentage of adults in the state with less than a high school diploma.",
                      "The percentage of adults in the state with a high school diploma only.",
                      "The percentage of adults in the state completing some college or associate's degree.",
                      "The percentage of adults in the state with a bachelor's degree or higher.",
                      "The number of COVID-19 cases in the state.",
                      "The number of COVID-19 deaths in the state.",
                      "The number of COVID-19 vaccine doses distributed in the state.",
                      "The number of COVID-19 vaccine doses administered in the state.")
    )
  }, rownames = FALSE)

  # Observe the plotGraph button and generate the line plot
  observeEvent(input$plotGraph, {
    # Load and prepare COVID data
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
        input$covidAttributeGraph == "Vaccine Distributed" ~ doses_distributed,
        input$covidAttributeGraph == "Vaccine Administered" ~ doses_administered,
        TRUE ~ NA_real_
      ), na.rm = TRUE)) %>%
      ungroup()

    # Load education data
    education_data <- read_csv("database/Education.csv") %>%
      group_by(state) %>%
      # Calculate the mean of each education level for each state
      summarise(Education_Level = mean(get(input$educationAttributeGraph), na.rm = TRUE)) %>%
      ungroup()

    # Merge education data with summarised COVID/vaccine data
    plot_data <- left_join(education_data, summarised_data, by = c("state" = "state"))

    # Generate the line plot
    if (nrow(plot_data) > 0) {
      output$linePlot <- renderPlot({
        ggplot(plot_data, aes(x = Education_Level, y = COVID_Attribute)) +
          geom_point() +
          # Add a linear regression line
          geom_smooth(method = "lm", se = FALSE, color = "red") +
          labs(x = input$educationAttributeGraph, y = input$covidAttributeGraph, title = "Education Level vs. COVID-19 Attribute") +
          theme_minimal() +
          scale_y_continuous(name = "COVID Attribute")
      })
      # Provide a message if no data is available for the selected criteria
    } else {
      output$linePlot <- renderPlot({
        ggplot() +
          geom_blank() +
          labs(title = "No data available for the selected criteria")
      })
    }
  })

  # Output for the top ten states with the highest doses administered per 100k
  output$topTenStatesPlot <- renderPlot({
    # Get the reactive vaccine data
    vaccine_data <- vaccine_data_reactive()
    # Generate the plot
    ggplot(vaccine_data, aes(x = reorder(state, doses_administered_per_100k), y = doses_administered_per_100k, fill = state)) +
      geom_col() +
      coord_flip() +
      labs(x = "State", y = "Doses Administered per 100,000", title = "Top 10 States by Doses Administered per 100k") +
      theme_minimal() +
      scale_fill_viridis_d() +
      scale_y_continuous(labels = label_comma())
  })

  # Output for the bubble chart about vaccine distribution and death
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
    ggplot(merged_data, aes(
      x = doses_distributed, y = doses_administered,
      size = population, color = Deaths
    )) +
      geom_point(alpha = 0.6) +
      scale_size(range = c(1, 20)) +
      scale_color_viridis_c() +
      labs(
        title = "Vaccines Distributed vs. Administered by State Population",
        x = "Doses Distributed",
        y = "Doses Administered",
        size = "Population",
        color = "Deaths"
      ) +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_x_continuous(labels = label_comma()) +
      scale_y_continuous(labels = label_comma())
  })
}

## Start the Shiny app

# Run the Shiny app
shinyApp(ui, server)
