setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table)
load('data/new/raw_data.RData')
object <- fread('data/object.csv',data.table=F)

nFeat <- as.matrix(aggregate(object$category,list(object$course_id),FUN=table))
colnames(nFeat) <- c('course_id',paste0('cs_',sub("x.","",colnames(nFeat)[-1])))
# for(i in 1: ncol(nFeat[,2])){
#     print(paste0(colnames(nFeat[,2])[i],': ',length(table(nFeat[,2][,i]))))
# }
nFeat <- nFeat[,-c(5,6)]

train <- merge(train,nFeat,sort=F,all.x=T)
test <- merge(test,nFeat,sort=F,all.x=T)

tail(train)
tail(test)

for(i in 59:71){
    train[,i] <- as.numeric(train[,i])
}
for(i in 58:70){
    test[,i] <- as.numeric(test[,i])
}

### dropout rate ###
nFeat <- aggregate(train$dropout,list(train$course_id),FUN=function(x) table(x)[2]/sum(table(x)))
colnames(nFeat) <- c('course_id','dropoutRate')
train <- merge(train,nFeat,sort=F,all.x=T)
test <- merge(test,nFeat,sort=F,all.x=T)

### num of course student attempt ###
all <- rbind(train[,-which(colnames(train) == 'dropout')], test)
nFeat <- aggregate(all$course_id,list(all$username),FUN=function(x) length(unique(x)))
colnames(nFeat) <- c('username','courseAttempt')
train <- merge(train,nFeat,sort=F,all.x=T)
test <- merge(test,nFeat,sort=F,all.x=T)


write.csv(train,file='data/new/train_extend.csv',quote=F, row.names=F)
write.csv(test,file='data/new/test_extend.csv',quote=F, row.names=F)
save(train, test, file='data/new/raw_data_extend.RData')
