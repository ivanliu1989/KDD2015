setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table)
# load(file='data/new/raw_data.RData')
load(file='data/new/raw_data_extend.RData')
### log transfer ###
train$DurationCourse <- as.numeric(train$DurationCourse); test$DurationCourse <- as.numeric(test$DurationCourse)
head(train[,-which(names(train) %in% c("enrollment_id","username","course_id","timeSkewness","timeKurtosis","dropout"))])
train[,-which(names(train) %in% c("enrollment_id","username","course_id","timeSkewness","timeKurtosis","dropout"))] <- 
    log1p(train[,-which(names(train) %in% c("enrollment_id","username","course_id","timeSkewness","timeKurtosis","dropout"))])

head(test[,-which(names(test) %in% c("enrollment_id","username","course_id","timeSkewness","timeKurtosis"))])
test[,-which(names(test) %in% c("enrollment_id","username","course_id","timeSkewness","timeKurtosis","dropout"))] <-
    log1p(test[,-which(names(test) %in% c("enrollment_id","username","course_id","timeSkewness","timeKurtosis","dropout"))])

head(train);head(test)

# save(train,test,file='data/new/raw_data_log.RData')
save(train,test,file='data/new/raw_data_log_extend.RData')
# load('data/new/raw_data_log.RData')

### scale ###
# all_df <- rbind(train[,-which(names(train) %in% c("enrollment_id","username","course_id","dropout"))], test[,-which(names(test) %in% c("enrollment_id","username","course_id"))])
# dim(train);dim(test);dim(all_df)
## scale(train[,2])
# all_df <- apply(all_df,2,rangeScale) 
# all_df <- apply(all_df,2,center_scale) 
# 
# train[,-which(names(train) %in% c("id","target"))] <- all_df[1:nrow(train),]
# test[,-which(names(test) %in% c("id"))] <- all_df[(nrow(train)+1):nrow(all_df),]
# head(train)
# head(test)
# 
# save(train,test,file='data/raw_data_log_scale_range_new_feat.RData')
