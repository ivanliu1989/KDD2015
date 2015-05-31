setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);require(caret);require(doMC)
registerDoMC(core=3)
load('data/new/cv_data_log_extend.RData')
# load('data/new/cv_data_anscombe_extend.RData')
# load('data/new/cv_data_extend.RData')
source('KDD2015/Main/0_function.R')
# load('data/new/raw_data_log_extend.RData')

################
### training ###
################
train_df <- train[,-which(names(train) %in% c('course_id', 'enrollment_id', 'username'))]
val_df <- val[,-which(names(val) %in% c('course_id', 'enrollment_id', 'username'))]
test_df <- test[,-which(names(test) %in% c('course_id', 'enrollment_id', 'username'))]
set.seed(8)
train_df <- shuffle(train_df)
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
gbmGrid <-  expand.grid(interaction.depth=8,n.trees=500,shrinkage=0.01,n.minobsinnode=1)
# 8 | 500 | 0.01 | 4 | 0.8431495
model <- 'gbm'
gbmFit <- train(dropout ~ ., data = train_df[,c(3,55)], method = model, #[,c('dropout',varImp)]
                trControl = fitControl, preProc = c("center", "scale"),
                metric = "ROC",verbose =T,tuneGrid = gbmGrid )#tuneLength = 6,

pred <- predict(gbmFit, newdata = val_df, type = "prob")
# pred <- predict(gbmFit, newdata = val_df)
# levels(pred) <- c(0,1); pred <- as.matrix(pred)
# pred <- as.numeric(pred);pred <- cbind(pred,pred)

##################
### Validation ###
##################
target_val = val_df$dropout
score <- auc(pred, target_val);print(score)
# submission <- cbind(test$enrollment_id, pred[,2])
write.csv(pred, file=paste0('results/valPred_',model,'_',score,'.csv'),row.names=F, quote=F)
pred_a <- mean()

####################
### Variable imp ###
####################
gbmImp <- varImp(gbmFit, scale = T)
gbmImp$importance
plot(gbmImp, top = 80)

RocImp <- filterVarImp(x = train_df[, -55], y = train_df$dropout)
RocImp[order(RocImp[,1],decreasing = T),]
varImp <- row.names(RocImp[which(RocImp$Yes>0.5),])


