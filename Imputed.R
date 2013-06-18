setwd("H:/titanic")
library(imputation)
library(randomForest)
library(caret)

train <- read.table("train.csv", header=T, sep=",", stringsAsFactors=FALSE)
str(train)
test <- read.table("test.csv", header=T, sep=",", stringsAsFactors=FALSE)

numeric.cols <- c("pclass", "age", "sibsp", "parch", "fare")

train[,numeric.cols] <- kNNImpute(train[,numeric.cols], 10)$x
test[,numeric.cols] <- kNNImpute(test[,numeric.cols], 10)$x

write.table(train, "train_knn_imputed.csv", col.names=FALSE, row.names=FALSE, sep=",")
write.table(test, "test_knn_imputed.csv", col.names=FALSE, row.names=FALSE, sep=",") 