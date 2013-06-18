rm(list=ls()) 
library(randomForest)
#library(missForest)
library(imputation)

## Load Data
train_all <- read.csv("train.csv", header=TRUE, stringsAsFactors=FALSE ) 
test_all  <- read.csv("test.csv", header=TRUE,  stringsAsFactors=FALSE )
str(train_all) # Examine variable types

## Drop useless columns from the DF
train <- subset(train_all, select = c(survived, pclass, sex, age, fare, sibsp, cabin)) 
test <- subset(test_all, select = c(pclass, sex, age, fare, sibsp, cabin)) 
train <- droplevels(train)
test <- droplevels(test)
## Address NA values

# TRAIN Fare & Age convert 0 to NA
train$fare[ train$fare == 0 ] <- NA
train$age[ train$age == 0 ] <- NA

# TEST Fare & Age  convert 0 to NA
test$fare[ test$fare == 0 ] <- NA
test$age[ test$age == 0 ] <- NA

# TRAIN and TEST fill blank entries with "UNKNOWN" 
train$cabin[ train$cabin == "" ] <- "Unknown"
test$cabin[ test$cabin == "" ] <- "Unknown"

# How many NA values in each Dataframe?
colSums(is.na(train))
colSums(is.na(test))


#### Fare Train Manual Impute

#Explore Fare
#median(train$fare, na.rm = T)
#length(which(train$fare == 0)) # count of 0 entries
#length(which(is.na(train$fare) == "TRUE"))# count of NA entries
#aggregate(train$fare, list(train$pclass), FUN = median, na.rm = TRUE) # median fare by pclass

# Fill 0 entries with median of fares by pclass
#train$fare[ train$fare == 0 & train$pclass == 1] <- tapply(train$fare, train$pclass, FUN =  median)[1]
#train$fare[ train$fare == 0 & train$pclass == 2] <- tapply(train$fare, train$pclass, FUN =  median)[2]
#train$fare[ train$fare == 0 & train$pclass == 3] <- tapply(train$fare, train$pclass, FUN =  median)[3]

##### Fare Test Manual Impute

#Explore Fare
#median(test$fare, na.rm = T)
#length(which(test$fare == 0)) # TEST count of 0 entries
#length(which(is.na(test$fare) == "TRUE"))# TEST count of NA entries
#aggregate(test$fare, list(test$pclass), FUN = median, na.rm = TRUE) # median fare by pclass

#test$fare[ test$fare == 0 & test$pclass == 1] <- tapply(test$fare, test$pclass, FUN =  median)[1]
#test$fare[ test$fare == 0 & test$pclass == 2] <- tapply(test$fare, test$pclass, FUN =  median)[2]
#test$fare[ test$fare == 0 & test$pclass == 3] <- tapply(test$fare, test$pclass, FUN =  median)[3]
#test$fare[ is.na(test$fare) == "TRUE"] <- 14.4583
 
##### Age Train Manual Impute

#Explore Age
median(train$age,na.rm = T)
length(which(train$age == 0)) # count of 0 entries
length(which(is.na(train$age) == "TRUE"))# count of NA entries
aggregate(train$age, list(train$sex), FUN = median, na.rm = TRUE) # median age by sex

#train$age[ is.na(train$age) == "TRUE" & train$sex == "male" ] <- 29
#train$age[ is.na(train$age) == "TRUE" & train$sex == "female"] <- 27

#### Age TEST Manual Impute

length(which(test$age == 0)) # count of 0 entries
length(which(is.na(test$age) == "TRUE"))# count of NA entries
aggregate(test$age, list(test$sex), FUN = median, na.rm = TRUE) # median age by sex
test$age[ is.na(test$age) == "TRUE"] <- 27


## missForest train
#train <- missForest(train, verbose = TRUE)
#train <- train[1]
#train <- data.frame(train)
#str(train)

## missForest test
#test <- missForest(test, verbose = TRUE)
#test <- test[1]
#test <- data.frame(test)
#str(test)
## Randomforests

#labels <- as.factor(train$ximp.survived)


#train$cabin <- as.integer(train$cabin)# rf needs these as integers?
#test$cabin <- as.integer(test$cabin)

train <- train[,-7] # remove cabin column
test <- test[,-6] # remove cabin column

train$sex <- as.factor(train$sex)# rf needs these as integers?
train$sex <- as.integer(train$sex)# rf needs these as integers?

test$sex <- as.factor(test$sex)
test$sex <- as.integer(test$sex)

str(train)
str(test)

numeric.cols <- c("pclass", "age", "fare", "sibsp") #"parch" # has to be chr not factors

train[,numeric.cols] <- kNNImpute(train[,numeric.cols], 10)$x
test[,numeric.cols] <- kNNImpute(test[,numeric.cols], 10)$x

outcomes <- as.factor(train$survived)
train <- train[,-1] # remove survived column from dataframe



str(train)
str(test)

rf <- randomForest(train, outcomes, xtest = test, ntree = 15000, do.trace=100, importance = TRUE)

#forest <- ?randomForest(survived ~ sex.name + pclass + age + fare + fare.distance, data = train, ntree = 15000, importance = TRUE)

importance(rf)
varImpPlot(rf)
plot(rf)

predictions <- levels(outcomes)[rf$test$predicted]

write(predictions, file="prediction.csv", ncolumns=1) 
 



