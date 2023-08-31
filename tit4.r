#titanic regression analysis
library(ggplot2)
library(dplyr)
titanic= read.csv("titanic.csv")
#first look at the data 
glimpse(titanic)

titanic = subset(titanic, select=c(Survived, Pclass, Sex, Age))
View(titanic)
glimpse(titanic)

#missing variable table 


# Check for missing values
missing_values <- titanic %>%
  summarise_all(~ sum(is.na(.)))

# Create a table indicating missing values
missing_table <- data.frame(
  Variable = names(missing_values),
  Missing_Count = unlist(missing_values)
)

# Print the missing value table
print(missing_table)

#my cleaned data set 
# Load your Titanic dataset (assuming it's named 'titanic')

# Remove rows with missing values in the 'Age' variable
titanic_clean <- titanic %>%
  filter(!is.na(Age))

# Print the first few rows of the cleaned dataset
head(titanic_clean)

#create a new variable for gender
titanic_clean <- titanic_clean %>%
  mutate(gender = ifelse(Sex == "male", 1, 0))

#take a look at my new data set
glimpse(titanic_clean)


titanic_clean$gender <- as.factor(titanic_clean$gender)
titanic_clean$Pclass <- as.factor(titanic_clean$Pclass)

logit_model <- glm(Survived ~ Pclass + gender + Age, data = titanic_clean, family = binomial)


summary(logit_model)


#chat gpts 
titanic_clean$Survived <- factor(titanic_clean$Survived, levels = c(0, 1), labels = c("Not Survived", "Survived"))
titanic_clean$Pclass <- factor(titanic_clean$Pclass)
titanic_clean$gender <- factor(titanic_clean$gender, levels = c(0, 1), labels = c("female", "male"))
logistic_model <- glm(Survived ~ gender + Pclass + Age, data = titanic_clean, family = "binomial")
summary(logistic_model)

