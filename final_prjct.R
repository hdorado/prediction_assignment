
rm(list=ls())

library(caret)
library(ggplot2)
library(randomForest)
library(party)

dirFol <- "C:/Users/hadorado/Desktop/FINAL_PROGT/"

setwd(dirFol)

train.pml <- read.csv("pml-training.csv",row.names=1)
test.pml <- read.csv("pml-testing.csv",row.names=1)

str(train.pml)
summary(train.pml)

train.pml[train.pml == "#DIV/0!" | train.pml == "" ] <- NA

train.pml <- droplevels(train.pml)

missCount <- apply(train.pml,2,function(x){ sum(is.na(x)) })

train.pml.depur <- train.pml[,missCount==0]

dim(train.pml.depur)

summary(train.pml.depur)


train.pml.depur <- train.pml.depur[,-(1:6)]

set.seed(666)

inTrain   <- createDataPartition(train.pml.depur$classe, p=0.70, list=F)
trainData <- train.pml.depur[inTrain, ]
testData  <- train.pml.depur[-inTrain, ]



tr.model.rf <- train(classe~., method = "rf", data=trainData,trControl=trainControl(method="cv", 5))

tr.model.rp <- train(classe~., method = "rpart", data=trainData,trControl=trainControl(method="cv", 5))

tr.model.gb <- train(classe~., method = "gbm", data=trainData,trControl=trainControl(method="cv", 5))

save(tr.model.rf,tr.model.rp,tr.model.gb,file="models.RData")

confusionMatrix(predict(tr.model.rf,testData),testData$classe)


confusionMatrix(predict(tr.model.rp,testData),testData$classe)

confusionMatrix(predict(tr.model.gb,testData),testData$classe)

test.set <- read.csv("pml-testing.csv")

varsTrain <- names(trainData)

test.set.depur <- test.set[,varsTrain[-53]]

predict(tr.model.rf,test.set.depur)





i


ctable <- as.table(matrix(c(42, 6, 8, 28), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

mosaicplot(i$table)



plot(varImp(tr.model.rf),top=15)
