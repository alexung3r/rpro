# first of all install the data
install.packages("readxl")
library(readxl)
library(ggplot2)
library(dplyr)
data <- read_excel("india.csv.xls")
data <- read.csv("india.csv.xls")
# Convert the Date column to Date format
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")
str(data)
head(data)

# Load necessary packages
library(dplyr)
library(ggplot2)

###########################################
# change in time for Ndhra Pradesh
# Filter the data for a specific region
selected_region <- "Andhra Pradesh"
filtered_data <- data %>%
  filter(Region == selected_region)

# Convert the Date column to Date format
filtered_data$Date <- as.Date(filtered_data$Date, format = "%d-%m-%Y")

# Create a line plot of unemployment rate over time for the selected region
ggplot(data = filtered_data, aes(x = Date, y = Estimated.Unemployment.Rate....)) +
  geom_line() +
  labs(title = paste("Unemployment Rate Over Time for", selected_region),
       x = "Date", y = "Unemployment Rate")
###############################################

# Filter data for the date "2019-05-31"
thirtyfirst <- data %>%
  filter(Date == as.Date("2019-05-31", format = "%Y-%m-%d"))

# Print the filtered data
print(thirtyfirst)
###############################################
#some descriptives
library(ggplot2)
#compare the regions even if the data set is way to large 
# Create a box plot of unemployment rates for each region
ggplot(data = data, aes(x = Region, y = Estimated.Unemployment.Rate....)) +
  geom_boxplot() +
  labs(title = "Unemployment Rate by Region", y = "Unemployment Rate")

# Calculate the correlation matrix
cor_matrix <- cor(data[, c("Estimated.Unemployment.Rate....", "Estimated.Labour.Participation.Rate....", "Estimated.Employed")])
# Display the correlation matrix
print(cor_matrix)

# Summary statistics for unemployment rate and labor participation rate
summary(data$Estimated.Unemployment.Rate....)
summary(data$Estimated.Labour.Participation.Rate....)



