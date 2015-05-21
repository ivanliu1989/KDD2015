source("aggregation_functions.R")

library(plyr)
library(reshape2)
library(MASS)

object_enhanced <- read.csv("../data/object_enhanced.csv")
object_enhanced$group <- paste('count', object_enhanced$category, object_enhanced$root, object_enhanced$level_2, object_enhanced$tree_level, sep="-")

# log_train <- read.csv("../data/log_train_sample.csv")
log_train <- read.csv("../raw/log_train.csv")
log_train$time <- as.POSIXct(gsub('T', ' ', log_train$time), tz="GMT")
colnames(log_train)[colnames(log_train) == "object"] <- "module_id"

enroll_train <- read.csv("../raw/enrollment_train.csv")

log_with_enroll <- join(enroll_train, log_train, by=c("username", "course_id"))
log_extended <- join(log_with_enroll, object_enhanced, by="module_id")
# write.csv(log_extended, "../data/log_extended.csv", quote=FALSE, row.names=FALSE)

# remove some large objects from memory
rm(log_with_enroll)
rm(log_train)

# choose enrollment ids for training, cross validation and test sets
test_size = as.integer(0.2*nrow(enroll_train))
class_def <- rep(NA, nrow(enroll_train))
set.seed(1234321)
class_def[sample(which(is.na(class_def)), test_size)] <- "test"
class_def[sample(which(is.na(class_def)), test_size)] <- "cv"
class_def[is.na(class_def)] <- "train"

test_enroll_ids <- enroll_train$enrollment_id[class_def == "test"]
cv_enroll_ids <- enroll_train$enrollment_id[class_def == "cv"]
train_enroll_ids <- enroll_train$enrollment_id[class_def == "train"]

# create log_extended_sample.csv
set.seed(1234)
sample_ids <- sample(unique(enroll_train$enrollment_id), 1000)
log_extended_sample <- log_extended[log_extended$enrollment_id %in% sample_ids, ]
write.csv(log_extended_sample, "../data/log_extended_sample.csv", quote=FALSE, row.names=FALSE)

# construct group_count_features.csv
group_count_features <- dcast(log_extended, enrollment_id ~ group, length, value.var="group")
group_count_features_sample <- group_count_features[group_count_features$enrollment_id %in% sample_ids, ]
write.csv(group_count_features, "../data/group_count_features.csv", quote=FALSE, row.names=FALSE)

supplied_truth <- read.csv("../raw/truth_train.csv", header=FALSE)
colnames(supplied_truth) <- c("enrollment_id", "response")


construct_time_aggregates <- function(x) {
    t0 <- x$time[!is.na(x$time)]
    t <- if (length(t0) == 0) 0 else difftime(t0, min(t0), units="days")
    c(duration = max(t), mean = mean(t), median = median(t), sd = sd(t))
}
clean_time_aggregates <- function(time_features) {
    selector <- is.na(time_features$sd)
    time_features$mean[selector] <- 0
    time_features$median[selector] <- 0
    time_features$sd[selector] <- 0
    time_features$skew <- (time_features$mean - time_features$median) / time_features$sd
    time_features$skew[is.na(time_features$skew)] <- 0
    time_features
}

time_features <- construct_aggregated_features(
    log_extended_sample,
    "enrollment_id",
    c("event", "category"),
    colname_prefix="t",
    aggregation_fun = construct_time_aggregates,
    post_aggregation_fun = clean_time_aggregates
)

#detailed_time_features <- construct_aggregated_features(
#  log_extended_sample[!is.na(log_extended_sample$root), ],
#  "enrollment_id",
#  c("root"),
#  colname_prefix="td",
#  aggregation_fun = construct_time_aggregates,
#  post_aggregation_fun = clean_time_aggregates
#)
#
#count_features <- construct_aggregated_features(
#  log_extended_sample,
#  "enrollment_id",
#  c("category", "root"),
#  colname_prefix = "c"
#)




# add truth to data
stopifnot(nrow(supplied_truth) == length(unique(supplied_truth$enrollment_id)))
model_data_0 <- join(group_count_features, supplied_truth, by="enrollment_id")
model_data <- model_data_0


# start PCA
train_model_data_pre_pca_0 <- model_data[model_data$enrollment_id %in% train_enroll_ids, ]

non_pca_cols <- c(
    which(colnames(train_model_data_pre_pca_0) %in% c("enrollment_id", "response")),
    which(colSums(train_model_data_pre_pca_0) == 0)
)
pca_cols <- setdiff(1:ncol(train_model_data_pre_pca_0), non_pca_cols)

train_data_pre_pca <- train_model_data_pre_pca_0[, pca_cols]

# cv_data <- model_data[model_data$enrollment_id %in% cv_enroll_ids]
# test_data <- model_data[model_data$enrollment_id %in% test_enroll_ids]

pca_result <- prcomp(train_data_pre_pca, scale=TRUE)

summary(pca_result)

# 445 features explains 95% of the variance in the training set
pca_feature_data <- cbind(
    model_data[, "enrollment_id", drop=FALSE],
    predict(pca_result, model_data[, pca_cols])[, 1:445],
    model_data[, "response", drop=FALSE]
)

train_data <- pca_feature_data[pca_feature_data$enrollment_id %in% train_enroll_ids, ]
cv_data <- pca_feature_data[pca_feature_data$enrollment_id %in% cv_enroll_ids, ]

write.csv(train_data, "../data/gen_train_data.csv", quote=FALSE, row.names=FALSE)
write.csv(cv_data, "../data/gen_cv_data.csv", quote=FALSE, row.names=FALSE)

library(randomForest)