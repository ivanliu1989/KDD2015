setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(xgboost);require(data.table);require(ROCR);require(doMC)
load(file='complete_datasets.RData')
registerDoMC(cores=4)

target_train <- train$dropout; target_val <- validation$dropout
train = train[,-which(names(train) %in% c("enrollment_id","dropout"))] #train
validation = validation[,-which(names(validation) %in% c("enrollment_id"))] #train_test
# test = test[,-which(names(test) %in% c("enrollment_id"))] #test

y = target_train#xgboost take features in [0,numOfClass)
train <- as.matrix(train)
dtrain = matrix(as.numeric(train[,3:12]),nrow(train[,3:12]),ncol(train[,3:12]))
validation <- as.matrix(validation)
dvalidation = matrix(as.numeric(validation),nrow(validation),ncol(validation))
# test
auc <- function(predict, target) {
    rocr <- prediction(predict[, 2], target)
    roc <- performance(rocr, "tpr", "fpr")
    plot(roc, colorize = TRUE)
    performance(rocr, "auc")@y.values
}

for (i in 1:30){
    seeds <- 8
    set.seed(seeds) #<<============#
    param <- list("objective" = "binary:logistic",
                  "eval_metric" = "auc", 
                  "nthread" = 3, set.seed = 8, eta=0.1, gamma = 0.1, #<<============#
                  "num_class" = 9, max.depth=8, min_child_weight=3,
                  subsample=1, colsample_bytree = 1)
    #0.05, 0.8, 0.9 | 0.01, 0.7, 0.6
    cv.nround = 1000
    # 698
    
    ### Train the model ###
    bst = xgboost(param=param, data = dtrain, label = y, nround = cv.nround, missing = NaN)
    
    ### Pred Validation ###
    pred = predict(bst,dvalidation,missing=NaN)#, ntreelimit=1
    pred = matrix(pred,2,length(pred)/9)
    pred = t(pred)
    
    score <- auc(pred, target_val)
    
    print(score)
}