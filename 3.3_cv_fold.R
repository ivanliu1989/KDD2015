setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);require(caret)

train_log <- fread('data/train/log_extended_train.csv')
test_log <- fread('data/test/log_extended_test.csv')
