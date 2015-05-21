library(plyr)
library(reshape2)

construct_aggregated_features <- function(x, row_id, group_by, colname_prefix="", aggregation_fun=nrow, post_aggregation_fun=NULL, sep = "-") {
  if (length(colname_prefix) != 1 || class(colname_prefix) != "character") stop("paramater 'colname_prefix' must be a character vector of length 1")
  if (length(group_by) == 0 || class(group_by) != "character") stop("group_by must be a character vector of length at least 1")
  if (length(unique(group_by)) != length(group_by)) stop("group_by values must be unique")
  if (sum(colnames(x) %in% group_by) != length(group_by)) stop("group_by values must all be data frame column names")
  
  build_aggregation_labels <- function(colname_prefix, x, sep = sep) {
    l <- list(colname_prefix)
    l[2:(ncol(x)+1)] <- lapply(1:ncol(x), function(i) x[, i])
    l["sep"] <- sep
    do.call(paste, l)
  }
  
  result <- ddply(
    cbind(x, aggregation_group = build_aggregation_labels(colname_prefix, x[, group_by], sep)),
    as.quoted(c(row_id, "aggregation_group")),
    aggregation_fun
  )
  
  if (!is.null(post_aggregation_fun)) {
    result <- post_aggregation_fun(result)
  }
  feature_format(result)
}

feature_format <- function(x) {
  original_colnames <- colnames(x)
  if (ncol(x) > 3) {
    melted <- melt(x, id.vars=c("enrollment_id", "aggregation_group"), variable.name="measure_type", value.name="value")
    melted$aggregation_group <- paste(melted$measure_type, melted$aggregation_group, sep="-")
    melted$measure_type <- NULL
    x <- melted
  }
  colnames(x) <- c("row_id", "aggregation_group", "value")
  result <- dcast(x, row_id ~ aggregation_group, value.var="value")
  
  for (i in 2:ncol(result)) {
    result[is.na(result[, i]), i] <- 0
  }

  selector <- which(vapply(2:ncol(result), function(i) all(result[, i] == 0), TRUE)) + 1
  
  if (length(selector) > 0) {
    result <- result[, -selector]
  }

  colnames(result)[1] <- original_colnames[1]
  result
}