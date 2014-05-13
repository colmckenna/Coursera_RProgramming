complete <- function(directory, id = 1:332) {
    wd <- getwd()
    fn <- list.files(directory)
    setwd(directory)
    df <- data.frame(id=numeric(), nobs=numeric())
    
}