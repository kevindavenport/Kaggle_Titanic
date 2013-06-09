rm(list=ls()) 
library(randomForest)

train_all <- read.csv("train.csv", header=TRUE ) 
test_all  <- read.csv("test.csv", header=TRUE )
str(train_all) # Examine variable types

train <- subset(train_all, select = c(survived, pclass, sex, age, fare, sibsp)) #subseting data
str(train)

test <- subset(test_all, select = c(pclass, sex, age, fare, sibsp)) #subseting data 
str(test)

# fill missing fare entries with median binned by gender (should do by class next)
median(train$fare)
length(which(train$fare == 0)) # count how many '0' entries there is in fare column
table(train$fare)
aggregate(train$fare, list(train$sex), FUN = median, na.rm = TRUE) # average fare by sex


aggregate(train$fare, list(train$pclass), FUN = median, na.rm = TRUE) # average fare by sex

df[is.na(df$traits), "traits"] <- tapply(df$traits, df$group, mean,  
na.rm=TRUE)[ df[is.na(df$traits),"group"] ]

train[train$fare == 0, "fare"] <- tapply(train$fare, train$sex, median, na.rm=TRUE)[ df[train$fare == 0,"sex"] ]

train$fare <- ave( which(train$fare == 0) , train$sex, FUN = median) 



##  Disregard as there are no NA entries, but there is "0" entries
#train$fare[ is.na( train$fare) ] <- 0
#test$fare[ is.na( test$fare) ]   <- 0

# rf needs these as integers?
train$sex <- as.integer(train$sex)
test$sex <- as.integer(test$sex)

## NOTE: Consider doing median by class or gender and filling respectively 
# fill missing age with median value
median(test$age,na.rm = T)
test$age[ is.na( test$age) ]     <- 27
train$age[ is.na( train$age) ]   <- 27

labels <- as.factor(train$survived)
train <- train[,-1] # remove survived column from dataframe

rf <- randomForest(train, labels, xtest = test, ntree = 5000, do.trace=100)
importance(rf)
predictions <- levels(labels)[rf$test$predicted]

write(predictions, file="prediction3.csv", ncolumns=1) 
 



################## Garbage
attach(train)
detach(train)
fare.list <- train$fare
for (i in 1:length(fare.list)) {
  if ( train$sex == "female" & train$fare == 0) {
    train$fare <- 23
  }
}
