#starwars
library("tidyverse")
sw= as.data.frame(starwars)
View(sw)
colnames(sw)
unique(sw$hair_color)
# Assuming 'sw' is the name of your Star Wars dataset
 sw[sw$hair_color == "none",]
sw= subset(sw,select = -c(films,vehicles,starships))
#omit our missing values 
sw_omit=na.omit(sw)
View(sw_omit)

# Assuming data is your dataset
ggplot(sw_omit, aes(height, mass)) +
  geom_point() +
  labs(x = "Height", y = "Mass", title = "Scatter Plot of Height vs. Mass")

#shows the plot with my regression line
ggplot(sw_omit, aes(height,mass)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(x = "Height", y = "Mass", title = "Scatter Plot with Regression Line") +
  theme_minimal()  # Customize the theme if needed

#lets start my logistic regression
unique(sw_omit$gender)
#declare my variables first
# Recode 'gender' variable to binary (0 and 1)
#feminine is null sonst 1
sw_omit$gender_binary <- ifelse(sw_omit$gender == "feminine", 0, 1)

install.packages("stats")
library(stats)  # For logistic regression
# Fit logistic regression model
# Fit logistic regression model
model <- glm(gender_binary ~ height + mass, data = sw_omit, family = binomial)
# Summary of logistic regression results
summary(model)
# Exponentiate coefficients to get odds ratios
exp_coef <- exp(coef(model))

#incredible stuff
# Install and load the effects package
install.packages("effects")
library(effects)

# Create an effect object
effect_model <- allEffects(model)

# Plot the effect of height
plot(effect_model, "height")

# Plot the effect of mass
plot(effect_model, "mass")
#These plots will show you how the predicted probabilities change as height or mass varies, while keeping other variables constant.

#now lets start my predicted probability plot
# Create a sequence of height and mass values for prediction
new_data <- expand.grid(
  height = seq(min(sw_omit$height), max(sw_omit$height), length = 100),
  mass = seq(min(sw_omit$mass), max(sw_omit$mass), length = 100)
)

# Predict the probability of being masculine for each combination of height and mass
new_data$predicted_prob <- predict(model, newdata = new_data, type = "response")

# Create the plot
library(ggplot2)
ggplot(new_data, aes(x = height, y = mass, fill = predicted_prob)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Height", y = "Mass", title = "Predicted Probability of Being Masculine") +
  theme_minimal()

#This will create a heatmap-like plot where the color represents the predicted probability of being categorized as "masculine" based on different combinations of height and mass.

##
#residual plot
# Calculate residuals
residuals <- resid(model, type = "pearson")

# Create a residual plot
plot(sw_omit$height, residuals, xlab = "Height", ylab = "Residuals",
     main = "Residual Plot for Height")

##check sensitivity and specificity
# Install and load the pROC package
install.packages("pROC")
library(pROC)

# Calculate ROC curve
roc_curve <- roc(sw_omit$gender_binary, predict(model, type = "response"))

# Create ROC curve plot
plot(roc_curve, print.thres = "best", main = "ROC Curve")

#confusion matrix 
# Install and load the caret package
install.packages("caret")
library(caret)

# Create a confusion matrix
confusion_matrix <- confusionMatrix(predict(model, type = "response") > 0.5, sw_omit$gender_binary)

# Create confusion matrix plot
plot(confusion_matrix$table, main = "Confusion Matrix")

# Install and load the InformationValue package
install.packages("InformationValue")
library(InformationValue)

# Create a lift curve
lift_curve <- LiftCurve(sw_omit$gender_binary, predict(model, type = "response"))

# Create lift curve plot
plot(lift_curve, main = "Lift Curve")


