setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table)

submission <- fread('Data/sampleSubmission.csv')
head(submission); dim(submission)

object <- as.data.frame(fread('Data/object.csv'))
str(object)
head(object); dim(object)
table(object$category)

train_enrol <- as.data.frame(fread('Data/train/enrollment_train.csv'))
str(train_enrol); head(train_enrol)
train_log <- as.data.frame(fread('Data/train/log_train.csv'))
str(train_log); head(train_log)

train_target <- as.data.frame(fread('Data/train/truth_train.csv'))
str(train_target); head(train_target)

