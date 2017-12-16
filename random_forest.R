####### Call libraries #################
rm(list = ls()) ; gc()
library(ggplot2)
library(caret)          # For dummyVar and for data partitioning
library(xgboost)        # For regression tree
library(Matrix)         # For sparse matrix
library(Metrics)        # for rmse()
library(randomForest)
library(dplyr)          # For data manipulation
library(lubridate)

# Read files and process
setwd("D:/MBA_Year2/TERM 5/BIG DATA/project")
# Read input file
# Load data
train <- read.csv("train.csv",header = T)
test <- read.csv("test.csv",header = T)
View(train)
View(test)
str(train)
str(test)

#convert date field to separate columns
train$day<-as.factor(day(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$month<-as.factor(month(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$year<-as.factor(year(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$Open.Date <- NULL

test$day<-as.factor(day(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$month<-as.factor(month(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$year<-as.factor(year(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$Open.Date <- NULL
# Random forest



# Convert all factor columns to dummy variables Train data
dummy<-dummyVars( ~ City.Group, data = train )
train1<-data.frame(predict(dummy,newdata=train))
train<-data.frame(cbind(train, train1))
train$City.Group<-NULL
rm(train1)
train$Type <- as.numeric(train$Type)
train$City <- as.numeric(train$City)
#for Test data
dummy<-dummyVars( ~ City.Group, data = test )
test1<-data.frame(predict(dummy,newdata=test))
test<-data.frame(cbind(test, test1))
train$City.Group<-NULL
rm(test1)
test$Type <- as.numeric(test$Type)
test$City <- as.numeric(test$City)

# Construct regression tree model
# randomForest automatically constructs regression tree if target variable is continuous
model1 = randomForest(  revenue ~.,
                        data=train,
                        ntree = 180,
                        do.trace = TRUE
)

# Make predictions. 
pred1 = predict(model1,
                test,
                type = "response"
)


