setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);require(caret);require(doMC)
registerDoMC(core=4)
load('data/new/cv_data_log_extend.RData')
# load('data/new/raw_data_log_extend.RData')

### training ###
train_df <- train[,-which(names(train) %in% c('course_id', 'enrollment_id', 'username'))]
val_df <- val[,-which(names(val) %in% c('course_id', 'enrollment_id', 'username'))]
test_df <- test[,-which(names(test) %in% c('course_id', 'enrollment_id', 'username'))]

### convert target ###
train_df$dropout <- as.factor(train_df$dropout)
levels(train_df$dropout) <- c('No','Yes')

### modeling ###
fitControl <- trainControl(method = "adaptive_cv", number = 10, repeats = 5,
                           classProbs = TRUE, summaryFunction = twoClassSummary,
                           adaptive = list(min = 8,alpha = 0.05,
                                           method = "BT",complete = TRUE))
set.seed(825)
svmFit <- train(dropout ~ ., data = train_df,method = "gbm",
                trControl = fitControl, preProc = c("center", "scale"),
                tuneLength = 6,metric = "ROC",verbose =T)

pred <- predict(gbmFit, newdata = val_df, type = "prob")

### Validation ###
auc <- function(predict, target) {
    rocr <- prediction(predict[, 2], target)
    roc <- performance(rocr, "tpr", "fpr")
    plot(roc, colorize = TRUE)
    performance(rocr, "auc")@y.values
}
target_val = val_df$dropout
score <- auc(pred, target_val)
confusionMatrix(pred, target_val)
sensitivity(pred, target_val)

### Variable imp ###
gbmImp <- varImp(gbmFit, scale = FALSE)
plot(gbmImp, top = 20)