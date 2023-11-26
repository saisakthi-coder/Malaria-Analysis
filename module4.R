# Load the dataset (assuming it's in a CSV file)
data <- read.csv(file.choose())

View(data)
data <- na.omit(data)

dengue_data = data
# Load necessary libraries
library(reshape2)
library(ggplot2)

# Create a matrix of dengue cases by month and location
dengue_matrix <- dcast(dengue_data, MONTH ~ LOCATION, value.var = "MALE", fun.aggregate = sum)

# Convert to a numeric matrix
dengue_matrix_numeric <- data.matrix(dengue_matrix[, -1])  # Exclude the Month column

# Plot a heatmap with a color scale
heatmap(dengue_matrix_numeric, 
        Rowv = NA, 
        Colv = NA, 
        col = colorRampPalette(c("white", "blue"))(20),
        scale = "none",
        main = "Dengue Cases by Month and Location",
        xlab = "Location",
        ylab = "Month",
        labRow = rownames(dengue_matrix),
        labCol = colnames(dengue_matrix),
        key = TRUE,  # Include color scale
        key.title = "Number of Dengue Cases",  # Color scale title
        key.xlab = "Count",  # X-axis label for the color scale
        key.ylab = NULL)  # Y-axis label for the color scale (set to NULL for no label)

