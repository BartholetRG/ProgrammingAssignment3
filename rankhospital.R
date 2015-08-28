rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## filter to only the columns I need: hospital name, state,  outcomes in order of heart
    ## attack, heart failure, and pneumonia
    data <- data[, c(2, 7, 11, 17, 23)]
    ## DATA COLUMNS:
    ## hospital name: 1
    ## state: 2
    ## heart attack outcome: 3
    ## heart failure outcome: 4
    ## pneumonia outcome: 5

    ## Check that state and outcome are valid
    ## Does state exist in the data?
    validState <- state %in% data[, 2]
    if(!validState) {stop("invalid state")}
    ## Is this a valid outcome argument?
    validOutcome <- outcome %in% c("heart attack", "heart failure", "pneumonia")
    if(!validOutcome) {stop("invalid outcome")}
    
    ## Return hospital name in that state with ith (determined by num) 30-day death rate
    ## First filter data for the state requested in column 2
    data <- subset(data, State == state)
    ## if looking for "best", then we want the first entry
    if(num == "best"){num <- 1}
    ## if outcome is heart attack
    if (outcome == "heart attack"){
        ## convert outcome column to numeric
        data[, 3] <- as.numeric(data[, 3]) 
        ## strip out any hospitals with NA values for this outcome
        data <- subset (data, !is.na(data[, 3]))
        ## then sort by the outcome values first, then hospital name
        data <- data[order(data[, 3], data[, 1]), ]
        ## if looking for "worst", then we want the last entry
        if(num == "worst") {num <- nrow(data)}
        ## pick off the ith (determined by value of num) hospital
        data[num,1]
    } else if (outcome == "heart failure"){
        ## convert outcome column to numeric
        data[, 4] <- as.numeric(data[, 4]) 
        ## strip out any hospitals with NA values for this outcome
        data <- subset (data, !is.na(data[, 4]))
        ## then sort by the outcome values first, then hospital name
        data <- data[order(data[, 4], data[, 1]), ]
        ## if looking for "worst", then we want the last entry
        if(num == "worst") {num <- nrow(data)}
        ## pick off the ith (determined by value of num) hospital
        data[num,1]
    } else { ## outcome is pneumonia
        ## convert outcome column to numeric
        data[, 5] <- as.numeric(data[, 5]) 
        ## strip out any hospitals with NA values for this outcome
        data <- subset (data, !is.na(data[, 5]))
        ## then sort by the outcome values first, then hospital name
        data <- data[order(data[, 5], data[, 1]), ]
        ## if looking for "worst", then we want the last entry
        if(num == "worst") {num <- nrow(data)}
        ## pick off the name ith (determined by value of num) hospital
        data[num,1]
    }
}