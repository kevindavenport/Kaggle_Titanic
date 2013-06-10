rm(list=ls()) 
library(randomForest)

## Load Data
train_all <- read.csv("train.csv", header=TRUE ) 
test_all  <- read.csv("test.csv", header=TRUE )
str(train_all) # Examine variable types


## Subset Data
train <- subset(train_all, select = c(survived, pclass, sex, age, fare, sibsp)) 
str(train)

test <- subset(test_all, select = c(pclass, sex, age, fare, sibsp)) 
str(test)

## Address NA values

# Fare TRAIN
median(train$fare)
length(which(train$fare == 0)) # count of 0 entries
length(which(is.na(train$fare) == "TRUE"))# count of NA entries

aggregate(train$fare, list(train$pclass), FUN = median, na.rm = TRUE) # median fare by pclass


train$fare[ train$fare == 0 & train$pclass == 1] <- tapply(train$fare, train$pclass, FUN =  median)[1]
train$fare[ train$fare == 0 & train$pclass == 2] <- tapply(train$fare, train$pclass, FUN =  median)[2]
train$fare[ train$fare == 0 & train$pclass == 3] <- tapply(train$fare, train$pclass, FUN =  median)[3]

# Fare Test
median(test$fare, na.rm = T)
length(which(test$fare == 0)) # TEST count of 0 entries
length(which(is.na(test$fare) == "TRUE"))# TEST count of NA entries

aggregate(test$fare, list(test$pclass), FUN = median, na.rm = TRUE) # median fare by pclass

test$fare[ test$fare == 0 & test$pclass == 1] <- tapply(test$fare, test$pclass, FUN =  median)[1]
test$fare[ test$fare == 0 & test$pclass == 2] <- tapply(test$fare, test$pclass, FUN =  median)[2]
test$fare[ test$fare == 0 & test$pclass == 3] <- tapply(test$fare, test$pclass, FUN =  median)[3]

test$fare[ is.na(test$fare) == "TRUE"] <- 14.4583

# Age TRAIN
median(train$age,na.rm = T)
length(which(train$age == 0)) # count of 0 entries
length(which(is.na(train$age) == "TRUE"))# count of NA entries

aggregate(train$age, list(train$sex), FUN = median, na.rm = TRUE) # median age by sex

train$age[ is.na(train$age) == "TRUE" & train$sex == "male" ] <- 29
train$age[ is.na(train$age) == "TRUE" & train$sex == "female"] <- 27

# Age TEST
length(which(test$age == 0)) # count of 0 entries
length(which(is.na(test$age) == "TRUE"))# count of NA entries

aggregate(test$age, list(test$sex), FUN = median, na.rm = TRUE) # median age by sex

test$age[ is.na(test$age) == "TRUE"] <- 27

## Randomforests
train$sex <- as.integer(train$sex)# rf needs these as integers?
test$sex <- as.integer(test$sex)

labels <- as.factor(train$survived)
train <- train[,-1] # remove survived column from dataframe

rf <- randomForest(train, labels, xtest = test, ntree = 5000, do.trace=100)
importance(rf)
varImpPlot(rf)
plot(rf)

predictions <- levels(labels)[rf$test$predicted]

write(predictions, file="prediction5.csv", ncolumns=1) 
 



################## Garbage
attach(train)
detach(train)
fare.list <- train$fare
for (i in 1:length(fare.list)) {
  if ( train$sex == "female" & train$fare == 0) {
    train$fare <- 23
  }
}
