setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(xgboost);require(data.table);require(ROCR);require(doMC)
load(file='complete_datasets.RData')
registerDoMC(cores=4)

target_train <- as.factor(train$dropout); target_val <- as.factor(validation$dropout)
train = train[,-which(names(train) %in% c("enrollment_id","dropout"))] #train
validation = validation[,-which(names(validation) %in% c("enrollment_id"))] #train_test
# test = test[,-which(names(test) %in% c("enrollment_id"))] #test

y = as.factor(target_train)#xgboost take features in [0,numOfClass)
train <- as.matrix(train)[,3:21]
dtrain = matrix(as.numeric(train),nrow(train),ncol(train))
validation <- as.matrix(validation)[,3:21]
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
    cv.nround = 500
    # 698
    
    ### Train the model ###
    bst = xgboost(param=param, data = dtrain, label = y, nround = cv.nround, missing = NaN)
    
    ### Pred Validation ###
    pred = predict(bst,dvalidation,missing=NaN)#, ntreelimit=1
    pred2 = matrix(pred,2,length(pred)/9)
    pred2 = t(pred2)
    
    score <- auc(pred2, target_val)
    
    print(score)
}


