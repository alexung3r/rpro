#Parch (Number of Parents/Children Aboard):
#SibSp (Number of Siblings/Spouses Aboard):
test=read.csv("titanic.csv")
length(test)
names(test)

library(ggplot2)
library(tidyverse)
library(dplyr)
#delete all the missings
training=na.omit(test)
View(titanic)
#small
tit= subset(titanic,select = c(Age, SibSp, Parch, gender))


#subset my data set
titanic= subset(training,select = -c(Name,Ticket,Fare,Cabin,Embarked))
str(titanic)
#create a new dummy variable 
titanic$gender <- ifelse(titanic$Sex == "female", 0, 1)


#some descriptives
summary(titanic$Age)
summary(titanic$gender)
summary(titanic$Survived)

#survival analysis
library(survival)
surv_object <- Surv(time = titanic$Age, event = titanic$Survived)

# EDA - Create a histogram of Age distribution
ggplot(titanic, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "dodgerblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

#my correlation matrix
cor_matrix <- cor(titanic[, c("Age", "SibSp", "Parch", "gender")])
#I might face the problem of multicolinearity
#so I will check the VIF
# Load the necessary libraries
install.packages("car")
# Load the necessary libraries
library(car)

# Assuming 'data' is your data frame containing the variables
# Select the relevant numerical variables for which you want to calculate VIF
numerical_vars <- c("Age", "SibSp", "Parch", "gender")

# Calculate VIF values
vif_values <- sapply(numerical_vars, function(var) {
  model_formula <- as.formula(paste(var, "~ . - ", var))
  vif(lm(model_formula, data = tit))
})

# Print the VIF values
print(vif_values)

#In all cases, VIF values around 1 indicate that there's relatively low multicollinearity, 
#which is generally a good thing. VIF values below 5 are generally considered acceptable,
#and those above 10 may indicate high multicollinearity,
#which could potentially lead to problems with the interpretation and stability of your regression model.
#Based on the VIF values you've provided, 
#it seems that multicollinearity is not a significant concern among these variables. 
#This is a positive outcome, as it suggests that the variables are not overly redundant in explaining the variance in each other, 
#which can make your regression model more robust.

#short there are no problems with multicolinearity


# Group comparisons
table(titanic$Pclass, titanic$Survived)  # Compare Pclass and Survival

#some plots
#survival by class
ggplot(titanic, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  labs(title = "Survival by Pclass", x = "Pclass", y = "Count") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))

#some boxplots
ggplot(titanic, aes(x = factor(Pclass), y = Age, fill = factor(Survived))) +
  geom_boxplot() +
  labs(title = "Age Distribution by Pclass and Survival", x = "Pclass", y = "Age") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))

#correlation heat map
cor_matrix <- cor(titanic[, c("Age", "SibSp", "Parch", "gender")])
library(ggcorrplot)
ggcorrplot(cor_matrix, method = "circle", lab = TRUE)

#pairwise scatter plots
pairs(titanic[, c("Age", "SibSp", "Parch", "gender")])
##########################################################################################
#start with my regression analysis
str(titanic)

# Load the necessary libraries
library(dplyr)
library(glmnet)  # for logistic regression

# Filter out unwanted variables
titanic_filtered <- titanic %>%
  select(Survived, Pclass, Age, SibSp, Parch)
head(titanic_filtered)
# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(titanic_filtered), 0.7 * nrow(titanic_filtered))
train_data <- titanic_filtered[train_indices, ]
test_data <- titanic_filtered[-train_indices, ]

# Fit the logistic regression model
log_model <- glm(Survived ~ Pclass + Age + SibSp + Parch, data = train_data, family = "binomial")

# Print the summary of the model
summary(log_model)

# Make predictions on the test set
test_data$predicted_prob <- predict(log_model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions
threshold <- 0.5
test_data$predicted <- ifelse(test_data$predicted_prob >= threshold, 1, 0)

# Calculate accuracy
accuracy <- mean(test_data$predicted == test_data$Survived)
cat("Accuracy:", accuracy)
#is like the corretly predicted values.
##########################################################################################
#now lets plot my findings
# Load necessary libraries
library(pROC)

# Calculate ROC curve
roc_curve <- roc(test_data$Survived, test_data$predicted_prob)

# Create the ROC plot
plot(roc_curve, main = "ROC Curve", print.thres = "best")

#predicted vs actual plot
ggplot(test_data, aes(x = predicted_prob, y = Survived)) +
geom_point(aes(color = factor(predicted)), alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 5), se = FALSE) +
  labs(title = "Predicted vs. Actual Survival",
       x = "Predicted Probability",
       y = "Survived",
       color = "Predicted")

#confusion matrix visualisations
# Load necessary libraries

test_data$Survived <- as.factor(test_data$Survived)
test_data$predicted <- as.factor(test_data$predicted)
levels(test_data$predicted) <- levels(test_data$Survived)

library(caret)

# Create a confusion matrix
conf_matrix <- confusionMatrix(test_data$predicted, test_data$Survived)

# Create a confusion matrix heatmap
heatmap(conf_matrix$table, annot = TRUE, fmt = "d",
        col = c("white", "lightblue"),
        rownames = c("Predicted 0", "Predicted 1"),
        main = "Confusion Matrix Heatmap")


###########################################################################
#without training and test set
# Load the necessary libraries
library(dplyr)
library(glmnet)  # for logistic regression

# Assuming 'titanic' is your data frame containing the variables
# Filter out unwanted variables
titanic_filtered <- titanic %>%
  select(Survived, Pclass, Age, SibSp, Parch)

# Fit the logistic regression model using the entire dataset
log_model <- glm(Survived ~ Pclass + Age + SibSp + Parch, data = titanic_filtered, family = "binomial")

# Print the summary of the model
summary(log_model)


# Load necessary libraries
library(ggplot2)
library(broom)

# Extract coefficient information
coef_summary <- tidy(log_model)

# Create a coefficient plot with confidence intervals
ggplot(coef_summary, aes(x = term, y = estimate,ymin=1,ymax=10)) +
  geom_point() +
  geom_errorbar() +
  coord_flip() +
  labs(title = "Estimated Coefficients", x = "Predictor", y = "Coefficient Estimate")


#predicted probabilities
# Predict probabilities using the model
titanic_filtered$predicted_prob <- predict(log_model, newdata = titanic_filtered, type = "response")

# Create a predicted vs. actual plot
ggplot(titanic_filtered, aes(x = predicted_prob, y = Survived)) +
  geom_point(aes(color = factor(predicted_prob > 0.5)), alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  labs(title = "Predicted vs. Actual Survival",
       x = "Predicted Probability",
       y = "Survived",
       color = "Predicted")






