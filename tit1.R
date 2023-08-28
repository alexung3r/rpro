test=read.csv("titanic.csv")
length(test)
names(test)

library(ggplot2)
#variables I would like to keep
#Id Survived Pclass Sex Age

training= subset(test,select = -c(Name,SibSp,Parch,Ticket,Fare,Cabin,Embarked))
is.na(training) #are there missings
train= na.omit(training)
#later on you must do a imputation
str(train)

#apply
apply(train,2,max) # maximal values in meinem datensatz
train[train$Age=="0.42",] #nachdem Passanger gesucht der 0.42 Jahre alt war und überlebt hat
sapply(train,class)
sapply(train,unique)
sapply(train[c(1,3)], class) #schaut sich von anfang an schon nur die columns an und du kannst die dadurch einfach auswählen
#mean calculation with sapply
sapply(train, function(x) ifelse(is.numeric(x), mean(x),NA))
tapply(train$Survived,train$Sex, sum)
Age= train[,"Age"]
cut(Age,3,include.lowest = TRUE) 
#categorize my variables into three columns
difference= cut(Age,3,include.lowest = TRUE, labels = c("young","mid","old"))
#Men who are older then fifty
mid_man= train[train$Age>=50.00, c("Survived","Sex","Pclass")]
sapply(mid_man[1], mean) # wie viel Männer haben im Durchschnitt überlebt
#erstell ne copy von train
copy_train=train
head(train)
class_three= train[train$Pclass==3,c("Survived","Sex")]
#wie viel von den in der 3 klasse haben überlebt?
sapply(class_three, function(x) ifelse(is.numeric(x),mean(x),NA))

#bisschen plotten um Überblick zu bekommen
ggplot(train, aes(Age,Pclass)) + geom_point() 
str(train$Pclass) #is now a integer
#lets make it a factor
# Assuming you have already converted Pclass to a factor (categorical variable)
train$Pclass <- factor(train$Pclass)
ggplot(train,aes(Age,Pclass)) + geom_point() + geom_smooth(se=FALSE)