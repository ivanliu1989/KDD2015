setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);library(dplyr);library(reshape2);library(MASS)
train_log <- fread('data/train/log_extended_train.csv', data.table=F)
test_log <- fread('data/test/log_extended_test.csv', data.table=F)

### Time format ###
train_log$time <- strptime(train_log$time, '%Y-%m-%d %H:%M:%S')
train_log$start <- strptime(train_log$start, '%Y-%m-%d %H:%M:%S')
test_log$time <- strptime(test_log$time, '%Y-%m-%d %H:%M:%S')
test_log$start <- strptime(test_log$start, '%Y-%m-%d %H:%M:%S')

write.csv(train_log,file='data/new/log_train.csv',quote=F, row.names=F)
write.csv(test_log,file='data/new/log_test.csv',quote=F, row.names=F)
save(train_log, test_log, file='data/new/log_enhanced.RData')

### Aggregate Features ###
train <- fread('data/train/enrollment_train.csv', data.table=F) #120543
test <- fread('data/test/enrollment_test.csv', data.table=F) #80362

# x1    total duration (course)
nFeat <- aggregate(train_log$time,list(train_log$enrollment_id),FUN=function(x) max(x)-min(x))
colnames(nFeat) <- c('enrollment_id','DurationCourse')
train <- merge(train,nFeat,sort=F,all=T)
### x2	total duration (online)

# x3	Number of requests (Server)
# x4	Number of sessions (Browser)
nFeat <- as.matrix(aggregate(train_log$source,list(train_log$enrollment_id),FUN=table))
colnames(nFeat) <- c('enrollment_id',paste0(sub("x.","",colnames(nFeat)[-1]),'Num'))
train <- merge(train,nFeat,sort=F,all=T)

# x5	Number of active days

# x6	Number of page views

# x8	Number of video views
# x10	Number of forum views
# x11	Number of wiki views
# x12	Number of problem views
# x13	Number of page_close
# x14	Number of access
# x15	Number of navigate
nFeat <- as.matrix(aggregate(train_log$event,list(train_log$enrollment_id),FUN=table))
colnames(nFeat) <- c('enrollment_id',paste0(sub("x.","",colnames(nFeat)[-1]),'Num'))
train <- merge(train,nFeat,sort=F,all=T)

### x7    Number of page views per session
train$pagePerSession <- 
### x9    Number of video views per session
nFeat$videoPerSession <- 
    
# x16	Most common request time
# x17	Most active day
# x18	observed event variance
# x19	number collaborations
# x20	max observed event duration
# x21	total lecture duration
# x22	total wiki Duration
# x23	average number of problem percentile
# x24	average number of problem percent

train_log[which(train_log$enrollment_id %in% train[is.na(train$DurationCourse),1]),]
