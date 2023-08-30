# Us presidental election data set
election= read.csv("president.csv")
head(election)
#remove duplicates of the poll id 
index <- which(duplicated(election$poll_id))
election_no_duplicates <- election[-index, ]

#remove some columns
library(dplyr)

columns_to_remove <- c(
  "pollster_id", "pollster", "sponsor_ids", "sponsors",
  "pollster_rating_id", "pollster_rating_name", "state",
  "sponsor_candidate_id", "sponsor_candidate", "question_id",
  "subpopulation", "population", "tracking", "created_at",
  "notes", "url", "source", "internal", "partisan"
)

election_cleaned <- election_no_duplicates %>%
  select(-all_of(columns_to_remove))



str(election_cleaned)
######################################################################
# only focus on the numeric variables 
numeric_vars <- c("poll_id", "sample_size", "yes", "no", "alternate_answers")

summary_numeric <- summary(election_cleaned[, numeric_vars])
print(summary_numeric)

#some histograms 
library(ggplot2)

ggplot(election_cleaned, aes(x = yes)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of 'yes' Responses", x = "Responses", y = "Frequency")

ggplot(election_cleaned, aes(x = no)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribution of 'no' Responses", x = "Responses", y = "Frequency")

ggplot(election_cleaned, aes(x = alternate_answers)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Distribution of 'alternate_answers' Responses", x = "Responses", y = "Frequency")
###################################################################
#now I want to start with a time trend analysis 
library(ggplot2)

# Convert start_date and end_date to Date format
election_cleaned$start_date <- as.Date(election_cleaned$start_date, format = "%m/%d/%y")
election_cleaned$end_date <- as.Date(election_cleaned$end_date, format = "%m/%d/%y")

# Create a line plot for 'yes' and 'no' responses over time
ggplot(election_cleaned, aes(x = start_date)) +
  geom_line(aes(y = yes, color = "Yes")) +
  geom_line(aes(y = no, color = "No")) +
  labs(title = "Time Trend Analysis of Approval Ratings",
       x = "Date", y = "Percentage",
       color = "Response") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red")) +
  theme(legend.position = "top", legend.title = element_blank())
########################################################################
# so now lets try some forecasting
library(forecast)
# im interested in forecasting the yes approval rates 
# Convert start_date to Date format
election_cleaned$start_date <- as.Date(election_cleaned$start_date, format = "%m/%d/%y")

# Create a time series object
time_series <- ts(election_cleaned$yes, frequency = 365)  # Adjust frequency if needed

#now lets split our data sets into train and test data sets
train_size <- 3000  # Adjust the number of training data points

train_data <- time_series[1:train_size]
test_data <- time_series[(train_size + 1):length(time_series)]

#now the auto arima command should find the best model 
# Find optimal ARIMA parameters using auto.arima
best_arima <- auto.arima(train_data)

# Extract ARMA order from best_arima
arma_order <- best_arima$arma

# Train ARIMA model
arima_model <- Arima(train_data, order = c(arma_order[1], 1, arma_order[2]))

#now lets forecast
# Forecast using the trained ARIMA model
forecast_result <- forecast(arima_model, h = length(test_data))

# Plot the forecast
plot(forecast_result, main = "ARIMA Forecast")

# Calculate RMSE for evaluation
#I think I get a way better forecast model then the one which was produced here 
rmse <- sqrt(mean((forecast_result$mean - test_data)^2))
print(paste("RMSE:", rmse))



