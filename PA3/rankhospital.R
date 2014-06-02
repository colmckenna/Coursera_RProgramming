rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!state %in% data$State){
        stop("invalid state")
    }
    
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
    
    mv <- tapply(!data[[o]]=="Not Available",data$State,sum)[[state]]
    
    if (num == "worst"){
        num <- mv
    }
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num > mv){
        return(NA)
    }
    else {
    sub <- data[!data[[o]]=="Not Available" & data$State == state, ]
    sub <- sub[order(sub$Hospital.Name), ]
    sub$ranks  <- rank(as.numeric(sub[[o]]), ties.method="first")
    sub[sub$ranks==num, 2]
    }
}
