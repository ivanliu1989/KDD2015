generateDistribution <- function(x) {
    name='hourDist'
    gap=0.2
    x_wo_na <- x[!is.na(x)]
    qdist <- seq(gap,1, by = gap)
    if (length(x_wo_na)<(2*length(qdist))) {
        dist <- quantile(x_wo_na, qdist,na.rm = T)
    } else {
        x_wo_peaks <- x_wo_na[abs(x_wo_na-mean(x_wo_na,na.rm = TRUE)) 
                              < 5*sd(x_wo_na,na.rm = TRUE)]
        dist <- quantile(x_wo_peaks, qdist,na.rm = T)
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
        dist <- quantile(x_wo_na, qdist,na.rm = T)
    } else {
        x_wo_peaks <- x_wo_na[abs(x_wo_na-mean(x_wo_na,na.rm = TRUE)) 
                              < 5*sd(x_wo_na,na.rm = TRUE)]
        dist <- quantile(x_wo_peaks, qdist,na.rm = T)
    }
    names(dist) = paste(name,names(dist),sep='_')
    names(dist) = gsub("%", "_pct", names(dist))
    return(dist)
}
