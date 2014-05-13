pollutantmean <- function(directory, pollutant, id = 1:332) {
    wd <- getwd()
	fn <- list.files(directory)
    setwd(directory)
    
    p<-vector()
    for (i in id) {
		df <- read.csv(fn[i])
        p <- c(p,df[[pollutant]])
	}
    
    p <- p[!is.na(p)]
    setwd(wd)
    mean(p)
}