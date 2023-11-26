# Load the dataset (assuming it's in a CSV file)
data <- read.csv(file.choose())

View(data)
data <- na.omit(data)

dengue_data = data
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming 'dengue_data' is loaded with the provided dataset

# Group by month, mosquito species, and calculate the total dengue cases
mosquito_analysis <- dengue_data %>%
  group_by(MONTH, MOSQUITO) %>%
  summarise(total_cases = sum(MALE + FEMALE))

# Plotting the comparison of dengue cases for each mosquito species by month
ggplot(data = mosquito_analysis, aes(x = MONTH, y = total_cases, fill = MOSQUITO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Total Dengue Cases", title = "Dengue Cases by Mosquito Species") +
  theme_minimal()
################################################################################################################
# Stacked area chart for dengue cases over time by mosquito species
ggplot(data = dengue_data, aes(x = WEEK, y = MALE + FEMALE, fill = MOSQUITO)) +
  geom_area(position = "stack") +
  labs(x = "Week", y = "Total Dengue Cases", title = "Dengue Cases Over Time by Mosquito Species") +
  theme_minimal()
##################################################################################################################
# Violin plot for distribution of dengue cases by mosquito species
ggplot(data = dengue_data, aes(x = MOSQUITO, y = MALE + FEMALE, fill = MOSQUITO)) +
  geom_violin() +
  labs(x = "Mosquito Species", y = "Total Dengue Cases", title = "Distribution of Dengue Cases by Mosquito Species") +
  theme_minimal()
####################################################################################################################################
# Calculate total dengue cases per age category for each mosquito species
dengue_age_cases <- dengue_data %>%
  group_by(MOSQUITO, AGE.CATEGORY) %>%
  summarise(total_cases = sum(MALE) + sum(FEMALE))

# Grouped bar chart for dengue cases by mosquito species and age category
ggplot(data = dengue_age_cases, aes(x = MOSQUITO, y = total_cases, fill = factor(AGE.CATEGORY))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Mosquito Species", y = "Total Dengue Cases", title = "Dengue Cases by Mosquito Species and Age Category") +
  theme_minimal()
