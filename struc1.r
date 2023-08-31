# structure the data frames
#remove columns
columns_to_remove = c(
  "pollster_id", "pollster"
)

election_cleaned = election_no_duplicates %>% 
  select(-all_of(columns_to_remove))

#structure the data set with subset
titanic= subset(training,select = -c(Name,Ticket,Fare,Cabin,Embarked))
str(titanic)

#remove duplicates
#remove duplicates of the poll id 
index <- which(duplicated(election$poll_id))
election_no_duplicates <- election[-index, ]

#create a dummy variable for the logistic regression analysis
#create a new dummy variable 
titanic$gender <- ifelse(titanic$Sex == "female", 0, 1)

#how do I get a correlation matrix 
cor_matrix <- cor(titanic[, c("Age", "SibSp", "Parch", "gender")])

#imputation
#impute missing variables with their means
mean_unemployment <- mean(data$Estimated.Unemployment.Rate...., na.rm = TRUE)
data_imputed <- data
data_imputed$Estimated.Unemployment.Rate....[is.na(data_imputed$Estimated.Unemployment.Rate....)] <- mean_unemployment


#compute a logistic regression
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

