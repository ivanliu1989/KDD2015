# pred1 <- pred
# pred2 <- pred
# 
# head(pred1); head(pred2)
# 
# pred <- pred1
# pred[,2:3]<- (pred1[,2:3] + pred2[,2:3])/2
# score <- auc(pred[,2:3], target_val);print(score)


submission <- fread('data/sampleSubmission.csv', data.table = F)

prediction <- submission
prediction[,1] <- test$enrollment_id
prediction[,2] <- pred[,3]

submit <- merge(submission, prediction, by = 'V1'); submit[,2] <- NULL
write.table(submit, file='submission_gbm_0.87.csv', row.names = F, quote = F, col.names = F, sep = ',')
