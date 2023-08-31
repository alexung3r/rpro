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

