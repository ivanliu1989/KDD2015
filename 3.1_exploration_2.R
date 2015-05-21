setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
start.time <- Sys.time()
rowN <- as.numeric(commandArgs(TRUE))

KDDtransform <- function(mainD, logD, rowN) {
    library(e1071)
    m1 <- read.csv(mainD, header=T, stringsAsFactors=F)
    d0 <- read.csv(logD, header=T, stringsAsFactors=F)
    z <- as.matrix(aggregate(d0$source,list(d0$enrollment_id),FUN=table))
    colnames(z) <- c('enrollment_id',paste0(unique(d0$source),'Sum'))
    
    m1 <- merge(m1,z,sort=F,all=T)
    z <- as.matrix(aggregate(d0$event,list(d0$enrollment_id),FUN=table))
    colnames(z) <- c('enrollment_id',paste0(unique(d0$event),'Sum'))
    
    m1 <- merge(m1,z,sort=F,all=T)
    d0$POSIX <- strptime(d0$time, "%Y-%m-%dT%H:%M:%S")
    z <- as.matrix(aggregate(d0$POSIX,list(d0$enrollment_id),FUN=var))
    colnames(z) <- c('enrollment_id','timeS2')
    
    m1 <- merge(m1,z,sort=F,all=T)
    z <- aggregate(d0$POSIX,list(d0$enrollment_id),FUN=function(x) max(x)-min(x))
    colnames(z) <- c('enrollment_id','timeMaxMin')
    
    m1 <- merge(m1,z,sort=F,all=T)
    d0$hour <- as.numeric(format(d0$POSIX, "%H")) + as.numeric(format(d0$POSIX, "%M"))/60
    z <- as.matrix(aggregate(d0$hour,list(d0$enrollment_id),FUN=mean))
    colnames(z) <- c('enrollment_id','hourMean')
    
    m1 <- merge(m1,z,sort=F,all=T)
    z <- as.matrix(aggregate(as.numeric(d0$POSIX),list(d0$enrollment_id),FUN=skewness))
    colnames(z) <- c('enrollment_id','timeSkew')
    
    m1 <- merge(m1,z,sort=F,all=T)
    z <- as.matrix(aggregate(as.numeric(d0$POSIX),list(d0$enrollment_id),FUN=kurtosis))
    colnames(z) <- c('enrollment_id','timeKurt')
    m1 <- merge(m1,z,sort=F,all=T)
    return(m1)
}

trainData <- KDDtransform('data/train/enrollment_train.csv','data/train/log_train.csv', rowN)
dropData <- read.csv('data/train/truth_train.csv', header=F, col.names=c('enrollment_id','dropout'))
trainData <- merge(trainData, dropData, sort=F, all=T)
write.csv(trainData,'TrainData.csv', row.names=F)

testData <- KDDtransform('data/test/enrollment_test.csv','data/test/log_test.csv', rowN)
write.csv(testData,'TestData.csv', row.names=F)

save(trainData, testData, file='Martin_data.RData')
end.time <- Sys.time()
end.time - start.time