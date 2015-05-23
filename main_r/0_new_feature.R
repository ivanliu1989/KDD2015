setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);library(dplyr);library(reshape2);library(MASS);library(e1071)
train_log <- fread('data/train/log_train.csv', data.table=F)
test_log <- fread('data/test/log_test.csv', data.table=F)
object <- fread('data/object.csv',data.table=F)
object$children <- NULL

train_log <- merge(train_log, object, by.x = c('course_id','object'),
                      by.y = c('course_id','module_id'), all.x.x = T)
test_log <- merge(test_log, object, by.x = c('course_id','object'),
                      by.y = c('course_id','module_id'), all.x.x = T)

###################
### Time format ###
###################
train_log$time <- strptime(train_log$time, '%Y-%m-%dT%H:%M:%S')
train_log$date <- as.Date(train_log$time, '%Y-%m-%d %H:%M:%S')
train_log$start <- strptime(train_log$start, '%Y-%m-%dT%H:%M:%S')
test_log$time <- strptime(test_log$time, '%Y-%m-%dT%H:%M:%S')
test_log$date <- as.Date(test_log$time, '%Y-%m-%d %H:%M:%S')
test_log$start <- strptime(test_log$start, '%Y-%m-%dT%H:%M:%S')
train_log$wkday <- as.POSIXlt(train_log$date)$wday
test_log$wkday <- as.POSIXlt(test_log$date)$wday
train_log$weekend <- ifelse(train_log$wkday %in% c(0,6), 1, 0)
test_log$weekend <- ifelse(test_log$wkday %in% c(0,6), 1, 0)
train_log$hour <- as.numeric(format(train_log$time, "%H"))
test_log$hour <- as.numeric(format(test_log$time, "%H"))

##########################
### Aggregate Features ###
##########################
train <- fread('data/train/enrollment_train.csv', data.table=F) #120543
test <- fread('data/test/enrollment_test.csv', data.table=F) #80362

featureEngineering <- function(df_log, df){
    # x1    total duration (course)
    nFeat <- aggregate(df_log$time,list(df_log$enrollment_id),FUN=function(x) max(x)-min(x))
    colnames(nFeat) <- c('enrollment_id','DurationCourse')
    df <- merge(df,nFeat,sort=F,all.x=T)
    
    # x3	Number of requests (Server)
    # x4	Number of sessions (Browser)
    nFeat <- as.matrix(aggregate(df_log$source,list(df_log$enrollment_id),FUN=table))
    colnames(nFeat) <- c('enrollment_id',paste0(sub("x.","",colnames(nFeat)[-1]),'Num'))
    df <- merge(df,nFeat,sort=F,all.x=T)
    
    # x5	Number of active days
    nFeat <- as.matrix(aggregate(df_log$date,list(df_log$enrollment_id),
                                 FUN=function(x) length(unique(x))))
    colnames(nFeat) <- c('enrollment_id','activeDays')
    df <- merge(df,nFeat,sort=F,all.x=T)
    
    # x8	Number of video views
    # x10	Number of forum views
    # x11	Number of wiki views
    # x12	Number of problem views
    # x13	Number of page_close
    # x14	Number of access
    # x15	Number of navigate
    nFeat <- as.matrix(aggregate(df_log$event,list(df_log$enrollment_id),FUN=table))
    colnames(nFeat) <- c('enrollment_id',paste0(sub("x.","",colnames(nFeat)[-1]),'Num'))
    df <- merge(df,nFeat,sort=F,all.x=T)
    
    # x7    Number of page views per session
    df$pagePerSession <- df$accessNum / df$activeDays
    # x9    Number of video views per session
    df$videoPerSession <- df$videoNum / df$activeDays
    # x6    Number of assignments per session
    df$assignmentPerSession <- df$problemNum / df$activeDays
    # x19    number collaborations
    df$collaborationNum <- df$discussionNum + df$wikiNum
    
    # x17	Most active day
    df_log$wkday <- as.factor(df_log$wkday)
    levels(df_log$wkday) <- c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')
    nFeat <- as.matrix(aggregate(df_log$wkday,list(df_log$enrollment_id),FUN=table))
    colnames(nFeat) <- c('enrollment_id',paste0(sub("x.","",colnames(nFeat)[-1]),'Num'))
    df <- merge(df,nFeat,sort=F,all.x=T)
    
    # x18	observed event variance
    nFeat <- as.matrix(aggregate(df_log$time,list(df_log$enrollment_id),FUN=sd))
    nFeat[is.na(nFeat[,2]),2] <- 0
    colnames(nFeat) <- c('enrollment_id', 'timeSD')
    df <- merge(df,nFeat,sort=F,all.x=T)
    
    # x16   Most common request time
    df_log$hour <- as.factor(df_log$hour)
    nFeat <- as.matrix(aggregate(df_log$hour,list(df_log$enrollment_id),FUN=table))
    colnames(nFeat) <- c('enrollment_id',paste0(sub("x.","",colnames(nFeat)[-1]),'oclock'))
    df <- merge(df,nFeat,sort=F,all.x=T)
    
    # x19   Time skewness
    nFeat <- as.matrix(aggregate(as.numeric(df_log$time),list(df_log$enrollment_id),FUN=skewness))
    nFeat[is.na(nFeat[,2]),2] <- 0
    colnames(nFeat) <- c('enrollment_id', 'timeSkewness')
    df <- merge(df,nFeat,sort=F,all.x=T)
    
    # x20   Time kurtosis
    nFeat <- as.matrix(aggregate(as.numeric(df_log$time),list(df_log$enrollment_id),FUN=kurtosis))
    nFeat[is.na(nFeat[,2]),2] <- 0
    colnames(nFeat) <- c('enrollment_id', 'timeKurtosis')
    df <- merge(df,nFeat,sort=F,all.x=T)
    
    # x22   Object event num
    nFeat <- as.matrix(aggregate(df_log$category,list(df_log$enrollment_id),FUN=table))
    colnames(nFeat) <- c('enrollment_id',paste0('cat_',sub("x.","",colnames(nFeat)[-1]),'Num'))
    df <- merge(df,nFeat,sort=F,all.x=T)
}

############
### main ###
############
train <- featureEngineering(train_log, train)
test <- featureEngineering(test_log, test)

###################
### Calibration ###
###################
target <- fread('data/train/truth_train.csv', data.table=F)
colnames(target) <- c('enrollment_id', 'dropout')
train <- merge(train,target,sort=F,all.x=T)

checkNull <- function (train){
    for(i in 1:ncol(train)){
        print(paste0(colnames(train)[i],': ',mean(is.na(train[,i]))))
    }    
}
checkNull(train)
checkNull(test)
# train[which(is.na(train[,9])),]
# train <- train[-which(is.na(train$dropout)),]

##################
### Save files ###
##################
write.csv(train,file='data/new/train.csv',quote=F, row.names=F)
write.csv(test,file='data/new/test.csv',quote=F, row.names=F)
save(train, test, file='data/new/log_enhanced.RData')
