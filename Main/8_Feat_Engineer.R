setwd('Google Drive/Competition/KDD2015')
rm(list = ls()); gc()
require(methods);require(data.table);library(h2o);library(caret)
load('data_new/raw_data_extend.RData')
options(scipen=3)
source('KDD2015/Main/0_function.R')

train <- train[,c(1:34,36:50,35)]
save(train,test,file='data_new/raw_data_extend.RData')

### Log ###
train$DurationCourse <- as.numeric(train$DurationCourse); test$DurationCourse <- as.numeric(test$DurationCourse)

head(train[,-which(names(train) %in% c("enrollment_id","username","course_id","dropout"))])
train[,-which(names(train) %in% c("enrollment_id","username","course_id","dropout"))] <- 
    log1p(train[,-which(names(train) %in% c("enrollment_id","username","course_id","dropout"))])

head(test[,-which(names(test) %in% c("enrollment_id","username","course_id"))])
test[,-which(names(test) %in% c("enrollment_id","username","course_id"))] <-
    log1p(test[,-which(names(test) %in% c("enrollment_id","username","course_id"))])

save(train,test,file='data_new/raw_data_log_extend.RData')

### CV & Train & Test ###
set.seed(9)
trainIndex <- createDataPartition(train$dropout, p = .8,list = FALSE,times = 1)
val <- train[-trainIndex,]
train <- train[trainIndex,]
dim(train);dim(val);dim(test)

save(train,val,test,file='data_new/cv_data_log_extend.RData')
