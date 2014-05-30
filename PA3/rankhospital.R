rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses = "character")
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
    else{
        num <- tapply(!data[[o]]=="Not Available",data$State,sum)[[state]]
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    sub <- data[!data[[o]]=="Not Available", ]
    order(sub)
    sub$myrank <- tapply(as.numeric(sub[[o]]),sub$State,rank, ties.method="first")
    
    sub[sub$myrank==num & sub$State==state, 2]
    
}
