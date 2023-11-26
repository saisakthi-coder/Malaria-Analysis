# Load necessary libraries
library(dplyr)
library(ggplot2)
library(ggcorrplot)

# Load the dataset
# Assuming the dataset is saved in a file named 'dengue_data.csv'
dengue_data <- read.csv(file.choose())

View(data)
data <- na.omit(data)

dengue_data = data

# Ensure the MONTH and WEEK columns are treated as factors and have the correct order
dengue_data$MONTH <- factor(dengue_data$MONTH, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
dengue_data$WEEK <- factor(dengue_data$WEEK, levels = unique(dengue_data$WEEK))

# Correlation Heatmap
correlation_matrix <- cor(dengue_data[, c("HUMIDITY", "MAX.TEMP", "AVG.TEMP", "MIN.TEMP", "DEW.POINT", "PRECIPITATION", "WIND", "MALE", "FEMALE")])
ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3)

# Pair Plot
pairs(dengue_data[, c("HUMIDITY", "MAX.TEMP", "AVG.TEMP", "MIN.TEMP", "DEW.POINT", "PRECIPITATION", "WIND", "MALE", "FEMALE")])

# Box Plots
# Reshape data from wide to long format
dengue_data_long <- dengue_data %>%
  pivot_longer(
    cols = c(HUMIDITY, MAX.TEMP, AVG.TEMP, MIN.TEMP, DEW.POINT, PRECIPITATION, WIND),
    names_to = "Weather_Factor",
    values_to = "Value"
  )

# Box Plots
ggplot(dengue_data_long, aes(x = Weather_Factor, y = Value, fill = factor(MONTH))) +
  geom_boxplot() +
  labs(x = "Weather Factor", y = "Value", fill = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violin Plots
ggplot(dengue_data_long, aes(x = Weather_Factor, y = Value, fill = factor(MONTH))) +
  geom_violin() +
  labs(x = "Weather Factor", y = "Value", fill = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
