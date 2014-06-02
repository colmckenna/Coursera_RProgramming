rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
        #sub <- data[!data[[o]]=="Not Available", ]
        sub <- data[with(data,order(data$State,data$Hospital.Name)), ]
        sub <- split(sub,tapply(as.numeric(sub[[o]]), sub$State, rank, ties.method="first"))
        sub
        #sub$ranks  <-
        #sub[sub$ranks==num, c("Hospital.Name", "State")]
    }
}
