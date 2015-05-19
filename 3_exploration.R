setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);require(caret)

train_log <- fread('data/train/log_extended_train.csv', data.table=F)
test_log <- fread('data/test/log_extended_test.csv', data.table=F)
train <- fread('data/train/enrollment_train.csv', data.table=F)
test <- fread('data/test/enrollment_test.csv', data.table=F)
target <- fread('data/train/truth_train.csv', data.table=F)
colnames(target) <- c('enrollment_id','dropout')

generateDistribution <- function(x,name,gap=0.2) {
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
    # Source - serverCount/browserCount
    newFeat <- as.matrix(aggregate(x_log$source,list(x_log$enrollment_id),FUN=table))
    colnames(newFeat) <- c('enrollment_id','browserCount','serverCount')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Event - navigateCount/accessCount/problemCount/page_closeCount/videoCount/discussionCount/wikiCount
    newFeat <- as.matrix(aggregate(x_log$event,list(x_log$enrollment_id),FUN=table))
    colnames(newFeat) <- c('enrollment_id','accessCount','discussionCount','nagivateCount',
                           'page_closeCount','problemCount','videoCount','wikiCount')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - Variance
    newFeat <- as.matrix(aggregate(x_log$time,list(x_log$enrollment_id),FUN=var))
    colnames(newFeat) <- c('enrollment_id','timeVar')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - Standard Deviation
    newFeat <- as.matrix(aggregate(x_log$time,list(x_log$enrollment_id),FUN=sd))
    colnames(newFeat) <- c('enrollment_id','timeSd')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - Duration
    newFeat <- as.matrix(aggregate(x_log$time,list(x_log$enrollment_id),FUN=function(x) max(x)-min(x)))
    colnames(newFeat) <- c('enrollment_id','timeDuration')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - hourDistribution
    x_log$hour <- as.numeric(format(x_log$time, "%H")) + as.numeric(format(x_log$time, "%M"))/60
    newfeat <- as.matrix(aggregate(x_log$hour,list(x_log$enrollment_id),FUN=mean))
    colnames(newfeat) <- c('enrollment_id','hourMean')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - skewnessTime
    newFeat <- as.matrix(aggregate(as.numeric(x_log$time),list(x_log$enrollment_id),FUN=skewness))
    colnames(newFeat) <- c('enrollment_id','timeSkew')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - kurtosisTime
    newFeat <- as.matrix(aggregate(as.numeric(x_log$time),list(x_log$enrollment_id),FUN=kurtosis))
    colnames(newFeat) <- c('enrollment_id','timeKurt')
    x <- merge(x, newFeat, sort=F, all=T)
    
    # Time - Frequency
    
    return(x)
}
