setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);require(caret)
source('KDD2015/2.3_feat_func.R')
### Read Data ###
train_log <- fread('data/train/log_train.csv', data.table=F)
test_log <- fread('data/test/log_test.csv', data.table=F)
train <- fread('data/train/enrollment_train.csv', data.table=F)
test <- fread('data/test/enrollment_test.csv', data.table=F)
target <- fread('data/train/truth_train.csv', data.table=F)
setnames(target, c("enrollment_id", "Dropout"))


featureEngineering <- function(x, x_log) {
    x_log$POSIX <- strptime(x_log$time, "%Y-%m-%dT%H:%M:%S")
    
    # Source - serverCount/browserCount
    newFeat <- as.matrix(aggregate(x_log$source,list(x_log$enrollment_id),FUN=table))
    colnames(newFeat) <- c('enrollment_id','browserCount','serverCount')
    x <- merge(x, newFeat, sort=F, all=F, all.x=T)
    
    # Event - navigateCount/accessCount/problemCount/page_closeCount/videoCount/discussionCount/wikiCount
    newFeat <- as.matrix(aggregate(x_log$event,list(x_log$enrollment_id),FUN=table))
    colnames(newFeat) <- c('enrollment_id','accessCount','discussionCount','nagivateCount',
                           'page_closeCount','problemCount','videoCount','wikiCount')
    x <- merge(x, newFeat, sort=F, all=F, all.x=T)
    
    # Time - Standard Deviation
    newFeat <- as.matrix(aggregate(x_log$POSIX,list(x_log$enrollment_id),FUN=sd,na.rm = TRUE))
    colnames(newFeat) <- c('enrollment_id','timeSd')
    x <- merge(x, newFeat, sort=F, all=F, all.x=T)
    
    # Time - Duration
    newFeat <- as.matrix(aggregate(x_log$POSIX,list(x_log$enrollment_id),
                                   FUN=function(x) max(x,na.rm = TRUE)-min(x,na.rm = TRUE)))
    colnames(newFeat) <- c('enrollment_id','timeDuration')
    x <- merge(x, newFeat, sort=F, all=F, all.x=T)
    
    # Time - hourDistribution/hourMean
    x_log$hour <- as.numeric(format(x_log$POSIX, "%H")) + as.numeric(format(x_log$POSIX, "%M"))/60
    newfeat <- as.matrix(aggregate(x_log$hour,list(x_log$enrollment_id),FUN=generateDistribution))
    colnames(newfeat) <- c('enrollment_id','hourDist_20','hourDist_40','hourDist_60','hourDist_80','hourDist_100')
    x <- merge(x, newFeat, sort=F, all=F, all.x=T)
    
    newfeat <- as.matrix(aggregate(x_log$hour,list(x_log$enrollment_id),FUN=mean,na.rm = TRUE))
    colnames(newfeat) <- c('enrollment_id','hourMean')
    x <- merge(x, newFeat, sort=F, all=F, all.x=T)
    
    # Time - skewnessTime
    newFeat <- as.matrix(aggregate(as.numeric(x_log$POSIX),list(x_log$enrollment_id),FUN=skewness,na.rm = TRUE))
    colnames(newFeat) <- c('enrollment_id','timeSkew')
    x <- merge(x, newFeat, sort=F, all=F, all.x=T)
    
    # Time - kurtosisTime
    newFeat <- as.matrix(aggregate(as.numeric(x_log$POSIX),list(x_log$enrollment_id),FUN=kurtosis,na.rm = TRUE))
    colnames(newFeat) <- c('enrollment_id','timeKurt')
    x <- merge(x, newFeat, sort=F, all=F, all.x=T)
    
    # Time - FrequencyDistribution/FreqMean
    newFeat <- as.matrix(aggregate(x_log$POSIX,list(x_log$enrollment_id),FUN=function(x) mean(diff(x),na.rm = T)))
    colnames(newFeat) <- c('enrollment_id','freqDist')
    x <- merge(x, newFeat, sort=F, all=F, all.x=T)
    
    newFeat <- as.matrix(aggregate(x_log$POSIX,list(x_log$enrollment_id),FUN=generateDistributionDiff))
    colnames(newFeat) <- c('enrollment_id','freqDist_20','freqDist_40','freqDist_60','freqDist_80','freqDist_100')
    x <- merge(x, newFeat, sort=F, all=F, all.x=T)
    
    return(x)
}

train_df <- featureEngineering(train, train_log)
train_df_2 <- merge(train_df, target, sort=F, all=F, all.y=T)
write.csv(train_df_2,'trainData.csv', row.names=F, quote=F)

test_df <- featureEngineering(test, test_log)
write.csv(test_df,'testData.csv', row.names=F, quote=F)

train_df <- train_df_2
save(train_df, test_df, file='raw_data.RData')
