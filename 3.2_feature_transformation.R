setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);require(caret)
load(file='raw_data.RData')
dim(test_df);dim(train_df)

username<- table(c(test_df$username, train_df$username))
username <- cbind(names(username),c(1:length(names(username))))
colnames(username) <- c('username', 'userID')
train_df <- merge(train_df, username, sort=F, all=F, all.x=T)
train_df$username <- NULL
test_df <- merge(test_df, username, sort=F, all=F, all.x=T)
test_df$username <- NULL

course_id <- table(c(test_df$course_id, train_df$course_id))
course_id <- cbind(names(course_id),c(1:length(names(course_id))))
colnames(course_id) <- c('course_id', 'courseID')
train_df <- merge(train_df, course_id, sort=F, all=F, all.x=T)
train_df$course_id <- NULL
test_df <- merge(test_df, course_id, sort=F, all=F, all.x=T)
test_df$course_id <- NULL

save(train_df, test_df, file='raw_data_encoded.RData')

### reorder columns ###
# train_df <- train_df[,c(1,22,23,2:21)]
# test_df <- test_df[,c(1,21,22,2:20)]

### Near Zero Var ###
nzv <- nearZeroVar(train_df, saveMetrics= TRUE)
nzv

### Correlation ###
descrCor <- cor(train_df[,4:22])
summary(descrCor[upper.tri(descrCor)])

### Linear Dependencies ###
comboInfo <- findLinearCombos(train_df[,4:22])
comboInfo

### Scale & center ###
preProcValues <- preProcess(train_df[,4:22], method = c("center", "scale"))

trainTransformed <- predict(preProcValues, train_df[,4:22])
testTransformed <- predict(preProcValues, test_df[,4:22])

train_df[,4:22] <- trainTransformed
test_df[,4:22] <- testTransformed

### CV & Train & Test ###
set.seed(9)
trainIndex <- createDataPartition(train_df$dropout, p = .8,list = FALSE,times = 1)
train <- train_df[trainIndex,]
validation <- train_df[-trainIndex,]
test <- test_df

save(train, test, validation, file='complete_datasets.RData')
