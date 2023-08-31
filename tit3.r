# titanic logistic regression and random forest
data= read.csv("titanic.csv")
library(dplyr)
library(ggplot2)
#lets take a look at the data
glimpse(data)
head(data)
View(data)
View(data_imputed)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(glm)


# Select the variables I want to vir wirth 
selected_vars <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Survived")
data_selected <- data %>% select(all_of(selected_vars))


# Check for missing values in the selected dataset
missing_values <- colSums(is.na(data_selected))

# Display the count of missing values for each column
print(missing_values)


# Create a bar plot of missing values
ggplot(data.frame(variable = names(missing_values), missing_values), aes(x = variable, y = missing_values)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Missing Values in Selected Dataset", x = "Variable", y = "Missing Value Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#It is obvius that I have missing variables for the age variable

#So thats why I will do an imputation
# Calculate the median age (excluding missing values)
median_age <- median(data_selected$Age, na.rm = TRUE)

# Impute missing age values with the median age
data_imputed <- data_selected
data_imputed$Age[is.na(data_imputed$Age)] <- median_age

# Check if missing values are imputed successfully
print(colSums(is.na(data_imputed)))
#now no more missing variables 
# data imputed is now my new data set 

#lets start with some descriptives first
summary(data_imputed)

#lets draw a histogram
ggplot(data_imputed, aes(x=Age)) +
  geom_histogram(binwidth = 5, fill="blue", color="black") +
  labs(title="Distribution of ages", x="Age", y="Frequency")

#now some bar plots
ggplot(data_imputed, aes(x=Pclass, fill=factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title="Survival by Passenger Class", x="Passenger Class", y="Count", fill="Survived")

ggplot(data_imputed, aes(x = Embarked, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Port of Embarkation", x = "Port of Embarkation", y = "Count", fill = "Survived")

#and to end with one box plot
ggplot(data_imputed, aes(x = factor(Pclass), y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  labs(title = "Fare Distribution by Passenger Class", x = "Passenger Class", y = "Fare", fill = "Passenger Class")


#my logistic regression analysis
# Create a new column "Survived_Category" based on "Survived" (0 = No, 1 = Yes)
data_imputed <- data_imputed %>%
  mutate(Survived_Category = ifelse(Survived == 1, "Yes", "No"))

# Convert categorical variables to factors
data_imputed$Sex <- as.factor(data_imputed$Sex)
data_imputed$Pclass <- as.factor(data_imputed$Pclass)

# Build logistic regression model
logit_model <- glm(Survived ~ Pclass + Sex + Age, data = data_imputed, family = binomial)

# Print model summary
summary(logit_model)

#For example, the coefficient for "Pclass2" is approximately -1.115. 
#This means that, compared to passengers in Pclass 1 (the reference category), 
#passengers in Pclass 2 are estimated to have a lower log odds of survival by about 1.115 units.
#This suggests that, compared to female passengers (the reference category), 
#male passengers are estimated to have a lower log odds of survival by about 2.611 units.
#The coefficient for "Age" is approximately -0.033. This implies that, for each one-unit increase in age,
#the log odds of survival decrease by about 0.033 units.

#now lets plot some of my findings
# lets start with my coefficient plot

coef_df <- data.frame(
  Variable = c("Intercept", "Pclass2", "Pclass3", "Sexmale", "Age"),
  Coefficient = coef(logit_model),
  CI_lower = coef(logit_model) - 1.96 * summary(logit_model)$coefficients[, "Std. Error"],
  CI_upper = coef(logit_model) + 1.96 * summary(logit_model)$coefficients[, "Std. Error"]
)

ggplot(coef_df, aes(x = Variable, y = Coefficient)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "red") +
  labs(title = "Coefficient Plot", y = "Coefficient Estimate") +
  coord_flip()

# and now my odds ratio plot : 
odds_ratio_df <- data.frame(
  Variable = c("Pclass2", "Pclass3", "Sexmale", "Age"),
  Odds_Ratio = exp(coef(logit_model)[-1]),
  CI_lower = exp(coef(logit_model)[-1] - 1.96 * summary(logit_model)$coefficients[-1, "Std. Error"]),
  CI_upper = exp(coef(logit_model)[-1] + 1.96 * summary(logit_model)$coefficients[-1, "Std. Error"])
)

ggplot(odds_ratio_df, aes(x = Variable, y = Odds_Ratio)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "red") +
  labs(title = "Odds Ratio Plot", y = "Odds Ratio") +
  coord_flip()

#now lets apply a random forest model
library(randomForest)

# Convert "Survived" to a factor (classification)
data_imputed$Survived <- as.factor(data_imputed$Survived)

# Define predictors and target variable
predictors <- c("Pclass", "Sex", "Age")
target <- "Survived"

# Convert categorical variables to factors
data_imputed$Sex <- as.factor(data_imputed$Sex)
data_imputed$Pclass <- as.factor(data_imputed$Pclass)

# Set a seed for reproducibility
set.seed(123)

# Split data into training and testing sets
train_index <- sample(1:nrow(data_imputed), nrow(data_imputed) * 0.8)
train_data <- data_imputed[train_index, ]
test_data <- data_imputed[-train_index, ]

# Build the Random Forest model
rf_model <- randomForest(formula(paste(target, "~", paste(predictors, collapse = "+"))),
                         data = train_data,
                         ntree = 500)

# Print the model
print(rf_model)

# Predict using the model
predictions <- predict(rf_model, newdata = test_data, type = "response") # type = "response" for classification

# Calculate accuracy
accuracy <- sum(predictions == test_data$Survived) / nrow(test_data)
print(paste("Accuracy:", accuracy))











