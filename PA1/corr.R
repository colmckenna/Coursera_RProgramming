corr <- function(directory, threshold = 0) {
    
    fn <- list.files(directory, full=TRUE)
    com <- complete(directory)
    ans <- vector()
    
    for (i in 1:nrow(com)) {
        if (com$nobs[i] > threshold) {
            temp <- read.csv(fn[i])
            ctemp <- temp[complete.cases(temp[[2]], temp[[3]]), ]
            ans <- c(ans, cor(ctemp$nitrate, ctemp$sulfate))
        }    
    }
    
    ans
}