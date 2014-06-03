rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- replace(data, "Not Available", NA)
    ## Check that state and outcome are valid
    vo <- list("heart attack"=11, "heart failure"=17, pneumonia=23)
    
    if(!outcome %in% names(vo)){
        stop("invalid outcome")    
    }
    
    o <- as.numeric(vo[[outcome]])
    
    if(!num %in% c("best","worst") & !is.numeric(num)){
        stop("invalid outcome")
    }
    
    if(num == "best"){
        num <- 1
    }
    
    mv <- sum(!data[[o]]=="Not Available")
    
    if (num == "worst"){
        num <- mv
    }
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num > mv){
        return(NA)
    }
    else {
        data[o=="Not Available"] <- NA
        sub <- data[with(data,order(data$State,data$Hospital.Name)), ]
        s <- split(sub, sub$State)
        #srank <- tapply(sub, sub$State,rank)
        #ans <- lapply(s,function(x) rank(x,x[[o]], ties.method="first"))
        #for( i in 1:length(s)){
        #    s[[i]]$rank <- rank(s[[o]],ties.method="first")
        #}
        s
        #head(ans)["AK"]
        #sub$ranks  <-
        #sub[sub$ranks==num, c("Hospital.Name", "State")]
    }
}
