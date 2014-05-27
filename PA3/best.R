best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    vo <- list("heart attack"=11, "heart failure"=17, pneumonia=23)
    o <- NA
    for (i in 1:length(names(vo))){
        if(outcome == names(vo)[i]){
            o <- as.numeric(vo[i])
        }
    }
    if (is.na(o)){
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    
    
    
    sub <- data[!data[o]=="Not Available", ]
    dr <- tapply(sub[[o]],sub[[7]],min)
    
    
    print(sub$State)
    print(state)
    print(sub[[o]])
    print(dr[[state]])
    
    sub[sub[[7]]==state && sub[[o]]==dr[[state]], ]
    
    
}
