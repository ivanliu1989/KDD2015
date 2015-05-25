require(ROCR)
auc <- function(predict, target) {
    rocr <- prediction(predict[, 2], target)
    roc <- performance(rocr, "tpr", "fpr")
    plot(roc, colorize = TRUE)
    performance(rocr, "auc")@y.values
}

Anscombe_Transform <- function(x){
    for(i in 1:ncol(x)){
        x[,i] <- as.numeric(x[,i])
        x[,i] <- sqrt(x[,i]+(3/8))
    }
    return(x)
}

shuffle <- function(sf){
    sf[,'id2'] <- sample(1:nrow(sf), nrow(sf), replace=T)
    sf <- sf[order(sf$id2),]
    sf[,'id2'] <- NULL
    return (sf)
}

rangeScale <- function(x){
    (x-min(x))/(max(x)-min(x))
}

center_scale <- function(x) {
    scale(x, scale = FALSE)
}