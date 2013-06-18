rm(list=ls()) 
library(MASS)
library(party)
library(e1071)
library(gbm)

## Load csv
train <- read.csv("train.csv", header = TRUE) 
test  <- read.csv("test.csv", header = TRUE)

## Data preprocessing

## Process cabin col later
drop.col <- c("name", "ticket", "sibsp", "parch", "cabin")
sub.train <- train[, !(names(train) %in% drop.col)]
sub.test <- test[, !(names(test) %in% drop.col)]
rm(drop.col)
## Convert factors to integers
sub.train$sex <- as.integer(sub.train$sex)
sub.test$sex <- as.integer(sub.test$sex)
sub.train$embarked <- as.integer(sub.train$embarked)
sub.test$embarked <- as.integer(sub.test$embarked)

## Addressing NAs in columns "age" and "fare"

# Where are the NAs?
colSums(is.na(sub.train))
colSums(is.na(sub.test))

## NA "fare" replaced w/median determined by "pclass"
sub.train.pclass.med <- tapply(sub.train$fare, sub.train$pclass, median, na.rm = T)
sub.test.pclass.med <- tapply(sub.test$fare, sub.test$pclass, median, na.rm = T)

#sub.train$fare[is.na(sub.train$fare)] <- sub.train.pclass.med[sub.train$pclass[which(is.na(sub.train$fare))]]
sub.test$fare[is.na(sub.test$fare)] <- sub.test.pclass.med[sub.test$pclass[which(is.na(sub.test$fare))]]

# Missing values in "age"

# Examine distribution of age and fill NA with rnorm based on known mean
truehist(sub.train$age)
truehist(sub.test$age)

set.seed(9) # CHANGE THIS?! -KD
sub.train.median.age <- median(train$age, na.rm = T)
sub.test.median.age <- median(test$age, na.rm = T)
sub.train.sd.age <- sd(train$age,na.rm =T)
sub.test.sd.age <- sd(test$age,na.rm =T)

sub.train.rnorm.ages <- floor(rnorm(length(which(is.na(train$age))), mean = sub.train.median.age, sd = sub.train.sd.age))
sub.test.rnorm.ages <- floor(rnorm(length(which(is.na(test$age))), mean = sub.test.median.age, sd = sub.test.sd.age))

sub.train$age[is.na(sub.train$age)] <- sub.train.rnorm.ages
sub.test$age[is.na(sub.test$age)] <- sub.test.rnorm.ages
############################## Consider doing this by pclass or gender?!?

## Modeling Begin

## formula1 for 'gender' model
##formula1 <- as.formula(as.factor(survived) ~ pclass + sex)
## formula2 for rest features model
##formula2 <- as.formula(as.factor(survived) ~ pclass + alone + fare + age)
predictions <- NULL
NT <- 1000
results <- NULL
## formula3 for 'gender' model using SVM
formula3 <- as.factor(survived) ~ pclass + sex
## formula1 and formula2 both for rest features without gender model
formula1 <- as.formula(as.factor(survived) ~ pclass  + sex + fare + age) # rm alone
formula2 <- as.formula(          survived  ~ pclass  + sex  + fare + age) # rm alone

## Train SVM(only for gender model) and Predict
formula3 <- as.factor(survived) ~ pclass + sex
tune <- tune.svm(formula3, data=sub.train, gamma=10^(-4:-1), cost=10^(1:4))
# summary(tune)
tune$best.parameters

model.svm <- svm(formula3, 
               data=sub.train, 
               type="C-classification", 
               kernel="radial", 
               probability=T, 
               gamma=tune$best.parameters$gamma, 
               cost=tune$best.parameters$cost)
predictions$svm <- as.numeric(predict(model.svm, newdata=sub.test))-1
ans3 <- as.numeric(predict(model.svm, newdata=sub.test))-1


## Train cForest and Predict
model.cforest <- cforest(formula2, data=sub.train, 
                           control=cforest_unbiased(ntree=NT, trace=F))
predictions$cforest<-predict(model.cforest, sub.test,OOB=T)
predictions$cforest[,1][predictions$cforest[,1]<=0.5] <- 0
predictions$cforest[,1][predictions$cforest[,1]>=0.5] <- 1
ans1 <- predictions$cforest[,1]

## Train ctree and Predict
##model.ctree <- ctree(formula1.cf, data=sub.train)
# plot(titanic.ctree)
##predictions$ctree <- as.numeric(predict(model.ctree, newdata=sub.test))-1

##ans <- as.numeric(predict(model.ctree, newdata=sub.test))-1
##write.csv(ans, file="submission-ctree.csv", row.names=F)

## Train gbm and Predict
model.gbm <- gbm(formula2, data=sub.train, n.trees=NT, interaction.depth=2)
predictions$gbm<-predict(model.gbm,sub.test, type = "response", n.trees = NT)
predictions$gbm <- round(predictions$gbm, 0)

ans2 <- predictions$gbm

## Ensembling models
ans <- (ans1 + ans2 + ans3)
ans[ans <= 1] <- 0
ans[ans == 1] <- 0
ans[ans >= 2] <- 1
ans[ans == 2] <- 1
write.csv(ans, file="differentSD.csv", row.names=F)
