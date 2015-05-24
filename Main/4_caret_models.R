setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);require(caret);require(doMC);require(ROCR)
registerDoMC(core=3)
load('data/new/cv_data_log_extend.RData')
# load('data/new/raw_data_log_extend.RData')

################
### training ###
################
train_df <- train[,-which(names(train) %in% c('course_id', 'enrollment_id', 'username'))]
val_df <- val[,-which(names(val) %in% c('course_id', 'enrollment_id', 'username'))]
test_df <- test[,-which(names(test) %in% c('course_id', 'enrollment_id', 'username'))]

######################
### convert target ###
######################
train_df$dropout <- as.factor(train_df$dropout)
levels(train_df$dropout) <- c('No','Yes')

################
### modeling ###
################
fitControl <- trainControl(method = "none", #number = 10, repeats = 5,
                           classProbs = TRUE, summaryFunction = twoClassSummary)#,
#adaptive = list(min = 8,alpha = 0.05,
#method = "BT",complete = TRUE))
gbmGrid <-  expand.grid(nrounds=500,max_depth=8,eta=0.1)
set.seed(8)
model <- 'xgbTree'
gbmFit <- train(dropout ~ ., data = train_df, method = model,
                trControl = fitControl, preProc = c("center", "scale"),
                metric = "ROC",verbose =T,tuneGrid = gbmGrid )#tuneLength = 6,

pred <- predict(gbmFit, newdata = val_df, type = "prob")

##################
### Validation ###
##################
# auc <- function(predict, target) {
#     rocr <- prediction(predict[, 2], target)
#     roc <- performance(rocr, "tpr", "fpr")
#     plot(roc, colorize = TRUE)
#     performance(rocr, "auc")@y.values
# }
# target_val = val_df$dropout
score <- auc(pred, target_val);print(score)

# submission <- cbind(test$enrollment_id, pred[,2])
write.csv(pred, file=paste0('results/valPred_',model,'_',score,'.csv'),row.names=F, quote=F)

pred_a <- mean()

####################
### Variable imp ###
####################
gbmImp <- varImp(gbmFit, scale = FALSE)
plot(gbmImp, top = 80)
