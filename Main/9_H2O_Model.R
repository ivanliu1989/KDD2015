setwd('Google Drive/Competition/KDD2015')
rm(list = ls()); gc()
require(methods);require(data.table);library(h2o)
load('data_new/cv_data_log_extend_40F.RData');options(scipen=3);source('KDD2015/Main/0_function.R')

################
### training ###
################
train_df <- train[,-which(names(train) %in% c('enrollment_id', 'username'))]#'course_id'
val_df <- val[,-which(names(val) %in% c('enrollment_id', 'username'))]
test_df <- test[,-which(names(test) %in% c('enrollment_id', 'username'))]
train_df$course_id <- as.factor(train_df$course_id)
val_df$course_id <- as.factor(val_df$course_id)
test_df$course_id <- as.factor(test_df$course_id)
# train_df <- train_df[,-which(names(train_df) %in% var_rm)]
# val_df <- val_df[,-which(names(val_df) %in% var_rm)]
# test_df <- test_df[,-which(names(test_df) %in% var_rm)]
train_df[,ncol(train_df)] <- as.factor(train_df[,ncol(train_df)])
levels(train_df[,ncol(train_df)]) <- c('No', 'Yes')
val_df[,ncol(val_df)] <- as.factor(val_df[,ncol(val_df)])
levels(val_df[,ncol(val_df)]) <- c('No', 'Yes')

# h2o.shutdown(localH2O)
localH2O <- h2o.init(nthread=3,Xmx="12g")

train.hex <- as.h2o(localH2O,train_df)
val.hex <- as.h2o(localH2O,val_df)
test.hex <- as.h2o(localH2O,test_df)

predictors <- 1:(ncol(train.hex)-1)
response <- ncol(train.hex)
target_val = val_df$dropout

for(i in 1:20){print(i)
    # deep learning
    model <- h2o.deeplearning(x=predictors,
                              y=response,
                              training_frame=train.hex,
                              overwrite_with_best_model = T,
                              # autoencoder=T,
                              # use_all_factor_levels=T,
                              activation = "RectifierWithDropout",
                              hidden = c(512,256,128),
                              epochs = 120,
                              train_samples_per_iteration = -1,
                              adaptive_rate = T,
                              rho = 0.99,
                              epsilon = 1.0e-5,
                              rate = 0.03,
                              rate_annealing = 1e-06,
                              rate_decay = 1,
                              momentum_start=0.5,
                              # momentum_ramp = 1e+06,
                              momentum_stable = 0.99,
                              nesterov_accelerated_gradient=T,
                              input_dropout_ratio = 0.15,
                              hidden_dropout_ratios = c(0.25,0.25,0.25),
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
                              # average_activation=T,
                              # sparsity_beta = 
                              seed=8)
    # Naive Bayes
    model <- h2o.naiveBayes(x=predictors,
                            y=response,
                            training_frame=train.hex,
                            laplace=0,#3
                            # threshold=0.01,
                            eps=0,
                            compute_metrics=T)
    # random forest
    model <- h2o.randomForest(x=predictors,
                              y=response,
                              training_frame=train.hex,
                              ntrees = 1000,
                              max_depth = 8,
                              # min_rows=1,
                              balance_classes=F,
                              mtries = 12,
                              sample_rate = 0.8,
                              build_tree_one_node=F,
                              nbins = 128,
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
                     ntrees=1300,
                     max_depth=5,
                     # min_rows=10,
                     learn_rate=0.01,
                     nbins=1024,
                     nbins_cats=524,
                     balance_classes=F,
                     seed=8)
    # model@model$scoring_history
    # model@model$variable_importances
    # var_imp <- as.data.frame(model@model$variable_importances)
    # var_rm <- var_imp[which(var_imp$percentage <= 0.005), 1]
    
    pred = as.data.frame(predict(model,val.hex))
    score <- auc(pred[,2:3], target_val);print(score)
    write.csv(pred, file=paste0('results/valPred_h2o_deeplearning_',score,'.csv'),row.names=F, quote=F)
    
    print(paste0('Model:',i,' Complete!'))     
}      
