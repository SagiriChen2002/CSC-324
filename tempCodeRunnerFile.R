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