# Load necessary libraries

# Load the dataset (assuming it's in a CSV file)
data <- read.csv("MALARIA.csv")

View(data)
data <- na.omit(data)
#########################################################################################################################################
# Load necessary libraries
library(dplyr)
# Load necessary libraries
library(ggplot2)

# Calculate counts for each combination
species_counts <- data %>%
  group_by(SPECIES, AGE.CATEGORY) %>%
  summarise(Count = n())

# Create a colorful palette for the plot
color_palette <- c("#FF6F61", "#6B4226", "#4B0082", "#FFD700", "#00FF00")  # Example colorful palette

# Create grouped bar plot with modified aesthetics
ggplot(species_counts, aes(x = factor(AGE.CATEGORY), y = Count, fill = SPECIES)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  labs(x = "Age Category", y = "Count", title = "Spread of Species by Age Category") +
  scale_fill_manual(values = color_palette, name = "Species") +  # Use the colorful palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####################################################################################################################################3
# Calculate counts for each Mosquito type
mosquito_counts <- data %>%
  summarise(
    Mosquito1_Count = sum(MOSQUITO1 == TRUE),
    Mosquito2_Count = sum(MOSQUITO2 == TRUE),
    Mosquito3_Count = sum(MOSQUITO3 == TRUE),
    Mosquito4_Count = sum(MOSQUITO4 == TRUE)
  )

# Print the counts
print(mosquito_counts)

# Reshape data for plotting
mosquito_counts_long <- mosquito_counts %>%
  pivot_longer(cols = starts_with("Mosquito"), names_to = "MosquitoType", values_to = "Count")

# Create a bar plot
ggplot(mosquito_counts_long, aes(x = MosquitoType, y = Count, fill = MosquitoType)) +
  geom_bar(stat = "identity") +
  labs(x = "Mosquito Type", y = "Count", title = "Distribution of Mosquitoes") +
  theme_minimal()

# Pie chart with outlines and enhanced aesthetics
ggplot(mosquito_counts_long, aes(x = "", y = Count, fill = MosquitoType)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.25) +  # Add outlines
  coord_polar("y") +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Distribution of Mosquitoes") +
  scale_fill_manual(values = c("#FF6F61", "#6B4226", "#4B0082", "#FFD700"), name = "Mosquito Type") +  # Custom colors
  guides(fill = guide_legend(reverse = TRUE))  # Reverse legend order for better visibility



# Calculate the percentage of each category
mosquito_counts_long <- mosquito_counts_long %>%
  group_by(MosquitoType) %>%
  mutate(Percentage = (Count / sum(mosquito_counts) )* 100)

#Spread of Mosquito Types in Different Places
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Calculate counts for each combination
mosquito_counts_location <- data %>%
  group_by(LOCATION) %>%
  summarise(
    Mosquito1_Count = sum(MOSQUITO1),
    Mosquito2_Count = sum(MOSQUITO2),
    Mosquito3_Count = sum(MOSQUITO3),
    Mosquito4_Count = sum(MOSQUITO4)
  )

# Reshape data for plotting
mosquito_counts_location_long <- mosquito_counts_location %>%
  pivot_longer(cols = starts_with("Mosquito"), names_to = "MosquitoType", values_to = "Count")

# Create grouped bar plot with adjusted aesthetics
ggplot(mosquito_counts_location_long, aes(x = LOCATION, y = Count, fill = MosquitoType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(x = "Location", y = "Count", title = "Spread of Mosquito Types in Different Places") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "green", "red", "purple"), name = "Mosquito Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
############################################################################################################################################

# Load necessary libraries
library(dplyr)

# Convert relevant columns to numeric
data$MAX.TEMP <- as.numeric(data$MAX.TEMP)
data$AVG.TEMP <- as.numeric(data$AVG.TEMP)
data$MIN.TEMP <- as.numeric(data$MIN.TEMP)

# Calculate correlation for each mosquito type
correlation_mosquito1 <- data %>%
  mutate(Mosquito1_Pv_Imported = as.numeric(MOSQUITO1 & MOSQUITO == "Pv" & SPECIES == "imported")) %>%
  select(Max_Temperature = MAX.TEMP, Avg_Temperature = AVG.TEMP, Min_Temperature = MIN.TEMP, Mosquito1_Pv_Imported) %>%
  cor()

correlation_mosquito2 <- data %>%
  mutate(Mosquito2_Pv_Indigenous = as.numeric(MOSQUITO2 & MOSQUITO == "Pv" & SPECIES == "indigenious")) %>%
  select(Max_Temperature = MAX.TEMP, Avg_Temperature = AVG.TEMP, Min_Temperature = MIN.TEMP, Mosquito2_Pv_Indigenous) %>%
  cor()

correlation_mosquito3 <- data %>%
  mutate(Mosquito3_Pf_Imported = as.numeric(MOSQUITO3 & MOSQUITO == "Pf" & SPECIES == "imported")) %>%
  select(Max_Temperature = MAX.TEMP, Avg_Temperature = AVG.TEMP, Min_Temperature = MIN.TEMP, Mosquito3_Pf_Imported) %>%
  cor()

correlation_mosquito4 <- data %>%
  mutate(Mosquito4_Pf_Indigenous = as.numeric(MOSQUITO4 & MOSQUITO == "Pf" & SPECIES == "indigenious")) %>%
  select(Max_Temperature = MAX.TEMP, Avg_Temperature = AVG.TEMP, Min_Temperature = MIN.TEMP, Mosquito4_Pf_Indigenous) %>%
  cor()

# Print correlation for each mosquito type
print("Correlation between Temperature and Mosquito 1 (Pv, Imported):")
print(correlation_mosquito1)

print("Correlation between Temperature and Mosquito 2 (Pv, Indigenous):")
print(correlation_mosquito2)

print("Correlation between Temperature and Mosquito 3 (Pf, Imported):")
print(correlation_mosquito3)

print("Correlation between Temperature and Mosquito 4 (Pf, Indigenous):")
print(correlation_mosquito4)

##########################################################################################################################################

# Load necessary libraries
library(dplyr)
library(tidyr)

# Create dummy variables for MOSQUITO and SPECIES
data1 <- data %>%
  mutate(MOSQUITO_Pv = as.numeric(MOSQUITO == "Pv"),
         MOSQUITO_Pf = as.numeric(MOSQUITO == "Pf"),
         SPECIES_imported = as.numeric(SPECIES == "imported"),
         SPECIES_indigenous = as.numeric(SPECIES == "indigenious"))

# Select relevant columns for correlation
cor_columns <- c("HUMIDITY", "MAX.TEMP", "AVG.TEMP", "MIN.TEMP", "DEW.POINT", 
                 "PRECIPITATION", "RAINY.DAYS", "WIND", "AGE.CATEGORY", 
                 "MALE", "FEMALE", "MOSQUITO_Pv", "MOSQUITO_Pf", 
                 "SPECIES_imported", "SPECIES_indigenous")

# Calculate the correlation matrix
cor_matrix <- cor(data1[, cor_columns])

# Visualization of the correlation matrix (heatmap)
library(ggplot2)
ggplot(data = as.data.frame(as.table(cor_matrix))) +
  geom_tile(aes(x = Var1, y = Var2, fill = Freq), color = "white") +
  theme_minimal() +
  labs(title = "Correlation Matrix",
       x = "Variables",
       y = "Variables",
       fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
#############################################################################################################3

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Calculate the correlation matrix
cor_matrix <- cor(data[, c("HUMIDITY", "MAX.TEMP", "AVG.TEMP", "MIN.TEMP", "DEW.POINT", "PRECIPITATION", "RAINY.DAYS", "WIND", "AGE.CATEGORY", "MALE", "FEMALE")])

# Customize color palette
color_palette <- c("#E63946", "#F1FAEE", "#A8DADC", "#457B9D")  # You can change these colors

# Visualization of the correlation matrix (heatmap)
ggplot(data = as.data.frame(as.table(cor_matrix)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = color_palette) +
  theme_minimal() +
  labs(title = "Correlation Matrix",
       x = "Variables",
       y = "Variables",
       fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.position = "right")  # Adjust legend position
############################################################################################################################################

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Convert relevant columns to numeric
data$MAX.TEMP <- as.numeric(data$MAX.TEMP)
data$AVG.TEMP <- as.numeric(data$AVG.TEMP)
data$MIN.TEMP <- as.numeric(data$MIN.TEMP)

# Calculate correlations for each mosquito type
correlation_mosquito1 <- cor(data[c("MAX.TEMP", "AVG.TEMP", "MIN.TEMP")], as.numeric(data$MOSQUITO1))
correlation_mosquito2 <- cor(data[c("MAX.TEMP", "AVG.TEMP", "MIN.TEMP")], as.numeric(data$MOSQUITO2))
correlation_mosquito3 <- cor(data[c("MAX.TEMP", "AVG.TEMP", "MIN.TEMP")], as.numeric(data$MOSQUITO3))
correlation_mosquito4 <- cor(data[c("MAX.TEMP", "AVG.TEMP", "MIN.TEMP")], as.numeric(data$MOSQUITO4))

# Create a data frame for visualization
correlation_df <- data.frame(
  MosquitoType = factor(c("Mosquito1", "Mosquito2", "Mosquito3", "Mosquito4")),
  Correlation = c(correlation_mosquito1, correlation_mosquito2, correlation_mosquito3, correlation_mosquito4)
)

# Plot with colorful heatmap and labels
ggplot(correlation_df, aes(x = MosquitoType, y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # Color gradient from blue to red
  labs(x = "Mosquito Type", y = "Correlation with Temperature", 
       title = "Correlation between Temperature and Mosquito Activity for Each Mosquito Type",
       fill = "Correlation") +  # Label for color legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "right")  # Position the legend on the right
##################################################################################################################################################################

# Load necessary libraries
library(ggplot2)

# Create a violin plot for temperature distribution by mosquito type
ggplot(data, aes(x = factor(MOSQUITO), y = MAX.TEMP, fill = SPECIES)) +
  geom_violin() +
  labs(title = "Temperature Distribution by Mosquito Type",
       x = "Mosquito Type",
       y = "Max Temperature",
       fill = "Species") +
  theme_minimal()
##############################################################################################################################
# Create a scatter plot for wind speed vs. dew point
ggplot(data, aes(x = WIND, y = DEW.POINT)) +
  geom_point() +
  labs(title = "Wind Speed vs. Dew Point",
       x = "Wind Speed",
       y = "Dew Point")

##############################################################################################
library(ggplot2)

# Assuming 'data' has columns 'AGE.CATEGORY', 'MALE', and 'FEMALE'
ggplot(data, aes(x = factor(AGE.CATEGORY), fill = factor(MALE))) +
  geom_bar(position = "stack") +
  labs(title = "Age Category vs. Male/Female Proportion",
       x = "Age Category",
       y = "Count",
       fill = "Male/Female")

