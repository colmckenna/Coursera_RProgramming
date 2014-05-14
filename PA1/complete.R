complete <- function(directory, id = 1:332) {
    fn <- list.files(directory, full=TRUE)
    df <- data.frame(id=numeric(), nobs=numeric())
    
    for (i in id) {
        temp <- read.csv(fn[i])
        obs <- nrow(temp[complete.cases(temp[[2]], temp[[3]]), ])
        nr <- data.frame(id=i,nobs=obs)
        df <- rbind(df,nr)
    }
    
    df
}