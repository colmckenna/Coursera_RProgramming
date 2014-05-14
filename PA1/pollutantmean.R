pollutantmean <- function(directory, pollutant, id = 1:332) {
	fn <- list.files(directory, full=TRUE)
    p<-vector()
    for (i in id) {
		df <- read.csv(fn[i])
        p <- c(p,df[[pollutant]])
	}
    p <- p[!is.na(p)]
    mean(p)
}