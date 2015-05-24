setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(methods);require(data.table);library(h2o)
load('data/new/cv_data_log_extend.RData')
options(scipen=3)

################
### training ###
################
train_df <- train[,-which(names(train) %in% c('course_id', 'enrollment_id', 'username'))]
val_df <- val[,-which(names(val) %in% c('course_id', 'enrollment_id', 'username'))]
test_df <- test[,-which(names(test) %in% c('course_id', 'enrollment_id', 'username'))]
train_df <- train_df[,c(1:54,56:68,55)]

localH2O <- h2o.init(nthread=3,Xmx="12g")
h2o.shutdown(localH2O)

train.hex <- as.h2o(localH2O,train_df)
val.hex <- as.h2o(localH2O,val_df)
test.hex <- as.h2o(localH2O,test_df)

predictors <- 1:(ncol(train.hex)-1)
response <- ncol(train.hex)

for(i in 1:20){
    print(i)
    model <- h2o.deeplearning(x=predictors,
                              y=response,
                              data=train.hex,
                              classification=T,
                              activation="RectifierWithDropout",
                              hidden=c(800,512,256),
                              hidden_dropout_ratio=c(0.25,0.25,0.25),
                              input_dropout_ratio=0.15,
                              epochs=100,
                              l1=1e-5,
                              l2=1e-5,
                              rho=0.99,
                              epsilon=1e-8,
                              train_samples_per_iteration=1000,
                              max_w2=10,
                              seed=8)
     
    pred = as.data.frame(h2o.predict(model,val.hex))
    score <- auc(pred[,2:3], target_val);print(score)
    write.csv(pred, file=paste0('results/valPred_h2o_deeplearning_',score,'.csv'),row.names=F, quote=F)
    
    print(paste0('Model:',i,' Complete!'))     
}      
pred1<-pred
pred2<-pred
pred_a <- (pred1+pred2)/2
