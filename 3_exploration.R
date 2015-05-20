setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);require(caret)

train_log <- fread('data/train/log_train.csv', data.table=F)
test_log <- fread('data/test/log_test.csv', data.table=F)
train <- fread('data/train/enrollment_train.csv', data.table=F)
test <- fread('data/test/enrollment_test.csv', data.table=F)
target <- fread('data/train/truth_train.csv', data.table=F)
colnames(target) <- c('enrollment_id','dropout')

generateDistribution <- function(x) {
    name='hourDist'
    gap=0.2
    x_wo_na <- x[!is.na(x)]
    qdist <- seq(gap,1, by = gap)
    if (length(x_wo_na)<(2*length(qdist))) {
        dist <- quantile(x_wo_na, qdist)
    } else {
        x_wo_peaks <- x_wo_na[abs(x_wo_na-mean(x_wo_na,na.rm = TRUE)) 
                              < 5*sd(x_wo_na,na.rm = TRUE)]
        dist <- quantile(x_wo_peaks, qdist)
    }
    names(dist) = paste(name,names(dist),sep='_')
    names(dist) = gsub("%", "_pct", names(dist))
    return(dist)
}
generateDistributionDiff <- function(x) {
    x <- as.numeric(diff(x))
    name='freqDist'
    gap=0.2
    x_wo_na <- x[!is.na(x)]
    qdist <- seq(gap,1, by = gap)
    if (length(x_wo_na)<(2*length(qdist))) {
        dist <- quantile(x_wo_na, qdist)
    } else {
        x_wo_peaks <- x_wo_na[abs(x_wo_na-mean(x_wo_na,na.rm = TRUE)) 
                              < 5*sd(x_wo_na,na.rm = TRUE)]
        dist <- quantile(x_wo_peaks, qdist)
    }
    names(dist) = paste(name,names(dist),sep='_')
    names(dist) = gsub("%", "_pct", names(dist))
    return(dist)
}

featureEngineering <- function(x, x_log) {
    x_log$POSIX <- strptime(x_log$time, "%Y-%m-%dT%H:%M:%S")
    
    # Source - serverCount/browserCount
    newFeat <- as.matrix(aggregate(x_log$source,list(x_log$enrollment_id),FUN=table))
    colnames(newFeat) <- c('enrollment_id','browserCount','serverCount')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Event - navigateCount/accessCount/problemCount/page_closeCount/videoCount/discussionCount/wikiCount
    newFeat <- as.matrix(aggregate(x_log$event,list(x_log$enrollment_id),FUN=table))
    colnames(newFeat) <- c('enrollment_id','accessCount','discussionCount','nagivateCount',
                           'page_closeCount','problemCount','videoCount','wikiCount')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - Standard Deviation
    newFeat <- as.matrix(aggregate(x_log$POSIX,list(x_log$enrollment_id),FUN=sd))
    colnames(newFeat) <- c('enrollment_id','timeSd')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - Duration
    newFeat <- as.matrix(aggregate(x_log$POSIX,list(x_log$enrollment_id),
                                   FUN=function(x) max(x)-min(x)))
    colnames(newFeat) <- c('enrollment_id','timeDuration')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - hourDistribution/hourMean
    x_log$hour <- as.numeric(format(x_log$POSIX, "%H")) + as.numeric(format(x_log$POSIX, "%M"))/60
    newfeat <- as.matrix(aggregate(x_log$hour,list(x_log$enrollment_id),FUN=generateDistribution))
    colnames(newfeat) <- c('enrollment_id','hourDist_20','hourDist_40','hourDist_60','hourDist_80','hourDist_100')
    x <- merge(x, newFeat, sort=F, all=T)
    
    newfeat <- as.matrix(aggregate(x_log$hour,list(x_log$enrollment_id),FUN=mean))
    colnames(newfeat) <- c('enrollment_id','hourMean')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - skewnessTime
    newFeat <- as.matrix(aggregate(as.numeric(x_log$POSIX),list(x_log$enrollment_id),FUN=skewness))
    colnames(newFeat) <- c('enrollment_id','timeSkew')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - kurtosisTime
    newFeat <- as.matrix(aggregate(as.numeric(x_log$POSIX),list(x_log$enrollment_id),FUN=kurtosis))
    colnames(newFeat) <- c('enrollment_id','timeKurt')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - FrequencyDistribution/FreqMean
    newFeat <- as.matrix(aggregate(x_log$POSIX,list(x_log$enrollment_id),FUN=function(x) mean(diff(x))))
    colnames(newFeat) <- c('enrollment_id','freqMean')
    x <- merge(x, newFeat, sort=F, all=T)
    
    newFeat <- as.matrix(aggregate(x_log$POSIX,list(x_log$enrollment_id),FUN=generateDistributionDiff))
    colnames(newFeat) <- c('enrollment_id','freqDist')
    x <- merge(x, newFeat, sort=F, all=T)
    
    return(x)
}
