# Load the data
# Assuming the dataset is loaded into a data frame named 'dengue_data'

# Load necessary libraries
library(dplyr)
library(tidyr)

data <- read.csv(file.choose())
data <- na.omit(data)
View(data)
dengue_data = data
# Data Preprocessing
# Convert relevant columns to appropriate data types
dengue_data$MONTH <- factor(dengue_data$MONTH, levels = c("January", "February", "March","April","May","June","July","August","September","October","November","December"))
dengue_data$WEEK <- as.integer(dengue_data$WEEK)

# Encode categorical variables
dengue_data <- dengue_data %>%
  mutate(MOSQUITO = ifelse(MOSQUITO == "Pv", 1, 0)) %>%
  mutate(SPECIES = ifelse(SPECIES == "imported", 1, 0)) %>%
  mutate(MOSQUITO1 = as.integer(MOSQUITO1)) %>%
  mutate(MOSQUITO2 = as.integer(MOSQUITO2)) %>%
  mutate(MOSQUITO3 = as.integer(MOSQUITO3)) %>%
  mutate(MOSQUITO4 = as.integer(MOSQUITO4))

# Check for missing values
sum(is.na(dengue_data))


# Load necessary libraries for modeling
library(caret)
install.packages("randomForest")
install.packages("glmnet")
library(randomForest)
library(glmnet)

# Feature Selection
# Let's select relevant features
selected_features <- c("HUMIDITY", "MOSQUITO", "SPECIES", "MAX.TEMP", "AVG.TEMP", "DEW.POINT", "PRECIPITATION", "RAINY.DAYS", "WIND", "AGE.CATEGORY")

# Subset the dataset with selected features
dengue_subset <- dengue_data[, c(selected_features, "MALE", "FEMALE")]

# Data Splitting
set.seed(123)  # for reproducibility
train_indices <- createDataPartition(dengue_subset$MALE, p = 0.7, list = FALSE)
train_data <- dengue_subset[train_indices, ]
test_data <- dengue_subset[-train_indices, ]

# Model Training - Random Forest
rf_model <- randomForest(as.factor(MALE) ~ ., data = train_data, ntree = 100)
rf_pred <- predict(rf_model, newdata = test_data, type = "response")

# Model Training - Logistic Regression
glm_model <- glm(MALE ~ ., data = train_data, family = binomial)
glm_pred <- predict(glm_model, newdata = test_data, type = "response")

# Model Evaluation
confusionMatrix(rf_pred, as.factor(test_data$MALE))
confusionMatrix(glm_pred, as.factor(test_data$MALE))

