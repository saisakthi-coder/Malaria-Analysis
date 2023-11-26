# Load the dataset (assuming it's in a CSV file)
data <- read.csv(file.choose())

View(data)
data <- na.omit(data)

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming the dataset is named 'dengue_data'
# Replace 'dengue_data.csv' with the actual file name if it's different
dengue_data <- data

# Convert 'MONTH' and 'WEEK' to factors with ordered levels
dengue_data$MONTH <- factor(dengue_data$MONTH, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
dengue_data$WEEK <- factor(dengue_data$WEEK, levels = unique(dengue_data$WEEK))

# Group by month and week, calculate the total dengue cases for each week in each month
dengue_incidence <- dengue_data %>%
  group_by(MONTH, WEEK) %>%
  summarise(total_cases = sum(MALE + FEMALE))

# Define colors for each month
month_colors <- c(
  "January" = "blue",
  "February" = "red",
  "March" = "green",
  "April" = "orange",
  "May" = "purple",
  "June" = "brown",
  "July" = "cyan",
  "August" = "magenta",
  "September" = "pink",
  "October" = "darkgreen",
  "November" = "gray",
  "December" = "yellow"
)

# Plotting the dengue incidence over months and weeks with improved aesthetics
ggplot(dengue_incidence, aes(x = WEEK, y = total_cases, group = MONTH, color = MONTH)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = month_colors) +
  labs(x = "Week", y = "Total Dengue Cases", title = "Dengue Incidence Patterns by Month and Week") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "top"
  )
#########################################################################################################################################

# Monthly Boxplot - Distribution of Dengue Cases

# Boxplot to show distribution of dengue cases by month
ggplot(dengue_data, aes(x = MONTH, y = MALE + FEMALE, fill = MOSQUITO)) +
  geom_boxplot() +
  labs(x = "Month", y = "Total Dengue Cases", title = "Distribution of Dengue Cases by Month and Mosquito Species") +
  theme_minimal() +
  theme(legend.title = element_blank())


######################################################################################################################################
#Heatmap - Dengue Incidence by Month and Week
# Load necessary libraries
library(reshape2)
library(ggplot2)

# Assuming 'dengue_data' and 'dengue_matrix' are loaded

# Convert 'MONTH' and 'WEEK' to factors with ordered levels
dengue_data$MONTH <- factor(dengue_data$MONTH, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
dengue_data$WEEK <- factor(dengue_data$WEEK, levels = unique(dengue_data$WEEK))

# Group by month and week, calculate the total dengue cases for each week in each month
dengue_incidence <- dengue_data %>%
  group_by(MONTH, WEEK) %>%
  summarise(total_cases = sum(MALE + FEMALE))

# Reshape data for heatmap
dengue_matrix <- acast(dengue_incidence, MONTH ~ WEEK, value.var = "total_cases", fill = 0)

# Convert data to long format for ggplot
library(reshape2)
melted_dengue_matrix <- melt(dengue_matrix, id.vars = "MONTH")

# Verify the structure of melted_dengue_matrix
str(melted_dengue_matrix)

# Plotting Heatmap for Dengue Incidence by Month and Week
library(ggplot2)
ggplot(data = melted_dengue_matrix, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Week", y = "Month", fill = "Total Dengue Cases", title = "Dengue Incidence Heatmap by Month and Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
######################################################################################################################################################################################################

# Load necessary libraries
library(dplyr)
library(ggplot2)

#Time Series Plot - Dengue Incidence Trend Over Time

# Assuming 'dengue_data' and 'dengue_matrix' are loaded

# Convert 'MONTH' and 'WEEK' to factors with ordered levels
dengue_data$MONTH <- factor(dengue_data$MONTH, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
dengue_data$WEEK <- factor(dengue_data$WEEK, levels = unique(dengue_data$WEEK))

# Group by month and week, calculate the total dengue cases for each week in each month
dengue_incidence <- dengue_data %>%
  group_by(MONTH, WEEK) %>%
  summarise(total_cases = sum(MALE + FEMALE))

# Convert to time series for time series plot
dengue_ts <- ts(dengue_incidence$total_cases, start = c(2023, 1), frequency = 52)

# Extract time and total cases for plotting
dengue_plot_data <- data.frame(time = seq.Date(as.Date("2023-01-01"), by = "week", length.out = length(dengue_ts)),
                               total_cases = as.numeric(dengue_ts))

# Plotting Time Series for Dengue Incidence Trend Over Time
ggplot(data = dengue_plot_data) +
  geom_line(aes(x = time, y = total_cases), color = "blue") +
  labs(x = "Time", y = "Total Dengue Cases", title = "Dengue Incidence Trend Over Time") +
  theme_minimal()
