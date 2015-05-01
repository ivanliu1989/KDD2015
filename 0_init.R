setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table)

submission <- fread('Data/sampleSubmission.csv')
head(submission); dim(submission)

object <- fread('Data/object.csv')
head(object); dim(object)
table(object$category)

train_enrol <- fread('Data/train/enrollment_train.csv')
str(train_enrol); head(train_enrol)
train_log <- fread('Data/train/log_train.csv')
str(train_log); head(train_log)
