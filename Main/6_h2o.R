setwd('Google Drive/Competition/KDD2015')
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
val_df <- val_df[,c(1:54,56:70,55)]
train_df[,70] <- as.factor(train_df[,70])
levels(train_df[,70]) <- c('No', 'Yes')
val_df[,70] <- as.factor(val_df[,70])
levels(val_df[,70]) <- c('No', 'Yes')

# h2o.shutdown(localH2O)
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
                              training_frame=train.hex,
                              overwrite_with_best_model = T,
                              autoencoder=T,
                              # use_all_factor_levels=T,
                              activation = "RectifierWithDropout",
                              hidden = c(512,256,128),
                              epochs = 100,
                              train_samples_per_iteration = -1,
                              adaptive_rate = T,
                              rho = 0.99,
                              epsilon = 1.0e-5,
                              rate = 0.01,
                              rate_annealing = 1e-06,
                              rate_decay = 1,
                              momentum_start=0.5,
                              # momentum_ramp = 1e+06,
                              momentum_stable = 0.99,
                              nesterov_accelerated_gradient=T,
                              input_dropout_ratio = 0.5,
                              hidden_dropout_ratios = 0.5,
                              l1 = 1e-05,
                              l2 = 1e-05,
                              loss = 'CrossEntropy',
                              classification_stop=0,
                              quiet_mode=F,
                              balance_classes=F,
                              diagnostics=T,
                              variable_importances=T,
                              fast_mode=F,
                              ignore_const_cols=T,
                              single_node_mode=T,
                              shuffle_training_data=T,
                              sparse=T,
                              #col_major
                              average_activation=T,
                              # sparsity_beta = 
                              seed=8)
    # Naive Bayes
    model <- h2o.naiveBayes(x=predictors,
                            y=response,
                            training_frame=train.hex,
                            laplace=0,#3
                            threshold=0.001,
                            # eps=0,
                            compute_metrics=T)
    # random forest
    model <- h2o.randomForest(x=predictors,
                              y=response,
                              training_frame=train.hex,
                              ntrees = 1500,
                              max_depth = 8,
                              min_rows=1,
                              balance_classes=F,
                              mtries = -1,
                              sample_rate = 0.7,
                              build_tree_one_node=F,
                              nbins = 20,
                              score_each_iteration = T,
                              balance_classes =F,
                              # nbins = 256,
                              binomial_double_trees =T,
                              # max_after_balance_size = 1,
                              # validation_frame = val.hex,
                              seed=8)
    # glm
    model <- h2o.glm(x=predictors,
                     y=response,
                     training_frame = train.hex,
                     max_iterations = 1000,
                     #beta_epsilon
                     #solver = 'L_BFGS',#IRLSM
                     #standardize = T,
                     family = 'binomial',
                     #link = 'logit', #log
                     #alpha = 0.5,
                     #prior = 1,
                     #lambda = 0.01,
                     #lambda_search = T,
                     #nlambdas
                     #lambda_min_ratio = 0.0001,
                     #offset_column
                     #weights_column
                     intercept=T
                     )
    # gbm
    model <- h2o.gbm(x=predictors,
                     y=response,
                     training_frame = train.hex,
                     distribution="bernoulli",
                     ntrees=900,
                     max_depth=8,
                     # min_rows=10,
                     learn_rate=0.01,
                     nbins=128,
                     #nbins_cats=1024,
                     balance_classes=F,
                     seed=8)
    model@model$scoring_history
    model@model$variable_importances
    #max_after_balance_size
    
    pred = as.data.frame(predict(model,val.hex))
    score <- auc(pred[,2:3], target_val);print(score)
    write.csv(pred, file=paste0('results/valPred_h2o_deeplearning_',score,'.csv'),row.names=F, quote=F)
    
    print(paste0('Model:',i,' Complete!'))     
}      
pred1<-pred
pred2<-pred
pred_a <- (pred1+pred2)/2
