setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(methods);require(data.table);library(h2o)
load('data/new/cv_data_log_extend.RData')
options(scipen=3)
source('KDD2015/Main/0_function.R')

################
### training ###
################
train_df <- train[,-which(names(train) %in% c('course_id', 'enrollment_id', 'username'))]
val_df <- val[,-which(names(val) %in% c('course_id', 'enrollment_id', 'username'))]
test_df <- test[,-which(names(test) %in% c('course_id', 'enrollment_id', 'username'))]
train_df <- train_df[,c(1:54,56:70,55)]

h2o.shutdown(localH2O)
localH2O <- h2o.init(nthread=3,Xmx="12g")

train.hex <- as.h2o(localH2O,train_df)
val.hex <- as.h2o(localH2O,val_df)
test.hex <- as.h2o(localH2O,test_df)

predictors <- 1:(ncol(train.hex)-1)
response <- ncol(train.hex)
target_val = val_df$dropout

for(i in 1:20){
    print(i)
    # deep learning
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
                              train_samples_per_iteration=2000,
                              max_w2=10,
                              #rate,
                              #rate_annealing=,
                              #rate_decay=,
                              momentum_start=0.5,
                              momentum_stable=0.99,
                              nesterov_accelerated_gradient=T,
                              loss='CrossEntropy',
                              shuffle_training_data=T,
                              seed=8)
    # random forest
    model <- h2o.randomForest(x=predictors,
                              y=response,
                              data=train.hex,
                              classification=T,
                              ntree = 1500,
                              depth = 8,
                              mtries = 8,
                              #sample.rate = 0.8,
                              nbins = 20,
                              importance = T,
                              score.each.iteration = T,
                              validation = val.hex,
                              balance.classes =F,
                              max.after.balance.size = 1,
                              verbose=T,
                              type='BigData',
                              stat.type = 'ENTROPY', #'GINI', 'TWOING'
                              seed=8)
    # gbm
    model <- h2o.gbm(x=predictors,
                     y=response,
                     data=train.hex,
                     distribution="bernoulli",
                     n.trees=500,
                     interaction.depth=8,
                     #n.minobsinnode=1,
                     shrinkage=0.1,
                     n.bins=9,
                     balance.classes=T)
    
    pred = as.data.frame(h2o.predict(model,val.hex))
    score <- auc(pred[,2:3], target_val);print(score)
    write.csv(pred, file=paste0('results/valPred_h2o_deeplearning_',score,'.csv'),row.names=F, quote=F)
    
    print(paste0('Model:',i,' Complete!'))     
}      
pred1<-pred
pred2<-pred
pred_a <- (pred1+pred2)/2
