# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Load the data
data <- read.csv("time_logs.csv")

# Split the 'TIME' column into 'Start' and 'End'
data <- data %>%
  mutate(StartTime = gsub(" - .*", "", TIME),
         EndTime = gsub(".*- ", "", TIME),
         Date = as.Date(StartTime, format="%m-%d-%Y"))

# Convert 'StartTime' and 'EndTime' to POSIXct
data <- data %>%
  mutate(StartTime = mdy_hm(paste(StartTime)),
         EndTime = mdy_hm(paste(substr(data$Start, 1, 11), EndTime)))

# Calculate the duration in hours or minutes
data <- data %>%
  mutate(Duration = as.numeric(difftime(EndTime, StartTime, units="hours")))

# View the modified data
head(data)
# Plotting the start times
ggplot(data, aes(x=Date, y=Duration)) +  # Replace 'Value' with the actual value column name
  geom_point() +  # Or geom_line() if you want to connect the points
  labs(title="Individual Project Time Log", x="Date", y="Time") +
  theme_minimal()
# Aggregate data to calculate average duration per week
aggregate_data <- aggregate(Duration ~ Date, data, mean, na.rm = TRUE)
# Create a bar plot
aggregate_data$FormattedDate <- format(aggregate_data$Date, "%m-%d")
y_max <- 6  # Adds 10% padding to the max value for better display
barplot(aggregate_data$Duration,
        names.arg = aggregate_data$FormattedDate,
        col = "blue",
        main = "Individual Project Time Log",
        xlab = "Date",
        ylab = "Average Duration (hours)",
        ylim = c(0, y_max),  # Adjust y-axis limits
        las = 2,  # Makes the axis labels perpendicular to the axis
        cex.names = 0.7)  # Adjusts the size of the week labels

weekly_duration <- aggregate(Duration ~ WEEK, data, sum)
weekly_duration$WeekLabel <- paste("Week", weekly_duration$WEEK)
ggplot(weekly_duration, aes(x="", y=Duration, fill=WeekLabel)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  labs(title="Project Weekly Working Time", x="", y="", fill="Week") +
  theme_void() +
  theme(legend.title = element_text(size=12), legend.text = element_text(size=10))
