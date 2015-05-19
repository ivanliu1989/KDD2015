setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);library(plyr);library(reshape2);library(MASS)
source("KDD2015/2_func.R")

# 0.load data
object_enhanced <- read.csv("data/object_enhanced.csv")
object_enhanced$group <- paste('count', object_enhanced$category, object_enhanced$root, 
                               object_enhanced$level_2, object_enhanced$tree_level, sep="-")
log_train <- fread("data/train/log_train.csv", data.table=F)
log_train$time <- as.POSIXct(gsub('T', ' ', log_train$time), tz="GMT")
colnames(log_train)[colnames(log_train) == "object"] <- "module_id"
enroll_train <- fread("data/train/enrollment_train.csv", data.table=F)
log_with_enroll <- join(enroll_train, log_train, by=c("username", "course_id"))
log_extended <- join(log_with_enroll, object_enhanced, by="module_id")
write.csv(log_extended, "data/train/log_extended_train.csv", quote=FALSE, row.names=FALSE)

log_test <- fread("data/test/log_test.csv", data.table=F)
log_test$time <- as.POSIXct(gsub('T', ' ', log_test$time), tz="GMT")
colnames(log_test)[colnames(log_test) == "object"] <- "module_id"
enroll_test <- fread("data/test/enrollment_test.csv", data.table=F)
log_with_enroll_test <- join(enroll_test, log_test, by=c("username", "course_id"))
log_extended_test <- join(log_with_enroll_test, object_enhanced, by="module_id")
write.csv(log_extended_test, "data/test/log_extended_test.csv", quote=FALSE, row.names=FALSE)

# 1.remove some large objects from memory
rm(log_with_enroll);rm(log_train);rm(log_with_enroll_test);rm(log_test)

# 2.construct group_count_features.csv
group_count_features <- dcast(log_extended, enrollment_id ~ group, length, value.var="group")
write.csv(group_count_features, "data/group_count_features.csv", quote=FALSE, row.names=FALSE)
supplied_truth <- read.csv("data/train/truth_train.csv", header=FALSE)
colnames(supplied_truth) <- c("enrollment_id", "response")

test_data <- dcast(log_extended_test, enrollment_id ~ group, length, value.var="group")
write.csv(test_data, "data/test.csv", quote=FALSE, row.names=FALSE)
# construct_time_aggregates <- function(x) {
#     t0 <- x$time[!is.na(x$time)]
#     t <- if (length(t0) == 0) 0 else difftime(t0, min(t0), units="days")
#     c(duration = max(t), mean = mean(t), median = median(t), sd = sd(t))
# }
# clean_time_aggregates <- function(time_features) {
#     selector <- is.na(time_features$sd)
#     time_features$mean[selector] <- 0
#     time_features$median[selector] <- 0
#     time_features$sd[selector] <- 0
#     time_features$skew <- (time_features$mean - time_features$median) / time_features$sd
#     time_features$skew[is.na(time_features$skew)] <- 0
#     time_features
# }
# 
# time_features <- construct_aggregated_features(
#     log_extended,
#     "enrollment_id",
#     c("event", "category"),
#     colname_prefix="t",
#     aggregation_fun = construct_time_aggregates,
#     post_aggregation_fun = clean_time_aggregates
# )
# detailed_time_features <- construct_aggregated_features(
#     log_extended[!is.na(log_extended$root), ],
#     "enrollment_id",
#     c("root"),
#     colname_prefix="td",
#     aggregation_fun = construct_time_aggregates,
#     post_aggregation_fun = clean_time_aggregates
# )
# count_features <- construct_aggregated_features(
#     log_extended,
#     "enrollment_id",
#     c("category", "root"),
#     colname_prefix = "c"
# )

# 3.add truth to data
stopifnot(nrow(supplied_truth) == length(unique(supplied_truth$enrollment_id)))
model_data_0 <- join(group_count_features, supplied_truth, by="enrollment_id")
train_data <- model_data_0
write.csv(train_data, "data/train.csv", quote=FALSE, row.names=FALSE)
