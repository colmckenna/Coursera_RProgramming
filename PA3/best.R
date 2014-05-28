best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid    
    if (!state %in% data$State){
        stop("invalid state")
    }
    
    vo <- list("heart attack"=11, "heart failure"=17, pneumonia=23)
    o <- NA
    if(outcome %in% names(vo)){
            o <- as.numeric(vo[[outcome]])
    }
    if (is.na(o)){
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    sub <- data[!data[[o]]=="Not Available", ]
    dr <- tapply(as.numeric(sub[[o]]),sub[[7]],min)
    
    
    min(sub[sub[[o]]==dr[[state]] & sub$State==state, 2])
}
