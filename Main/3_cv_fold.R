setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);require(caret)

load('data/new/raw_data_extend.RData')

### CV & Train & Test ###
set.seed(9)
trainIndex <- createDataPartition(train$dropout, p = .8,list = FALSE,times = 1)
val <- train[-trainIndex,]
train <- train[trainIndex,]
dim(train);dim(val);dim(test)

save(train,val,test,file='data/new/cv_data_log_extend.RData')
