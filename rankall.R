rankall <- function(outcome, num = "best") {
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

    ## ------------------------------
    ## Check that outcome is valid
    validOutcome <- outcome %in% c("heart attack", "heart failure", "pneumonia")
    if(!validOutcome) {stop("invalid outcome")}
    
    ## --------------------------------
    ## Return a data frame with the hospital names and the (abbreviated) state name
    
    ## if looking for "best", then we want the first entry
    if(num == "best"){num <- 1}
    
    ## split the data into dataframes corresponding to each state
    stateOutcomes <- split(data, data[, 2])
    ## note that the list of dataframes is now sorted alphabetically by state
    
    ## create an empty output data frame
    output <- data.frame(hospital = character(0), state = character(0))

    ##----------------------------------
    ## if outcome is heart attack
    if (outcome == "heart attack"){
        ## apply the following anonymous function to each list element (dataframe for each state)
        ## and write to the output variable (enumerated list of states, with vector of hospital and
        ## state for each element)
        output <- lapply(stateOutcomes, function(stateData) {
            ## convert outcome column to numeric
            stateData[, 3] <- as.numeric(stateData[, 3]) 
            ## capture the state before we potentially strip out all the rows in the dataframe
            ## with the command after this one
            thisState <- stateData[1,2]
            ## strip out any hospitals with NA values for this outcome
            stateData <- subset (stateData, !is.na(stateData[, 3]))
            ## if looking for "worst", then we want the last entry
            if(num == "worst") {num <- nrow(stateData)}
            ## then sort by the outcome values, then hospital name
            stateData <- stateData[order(stateData[, 3], stateData[, 1]), ]
            ## add the selected (num) row to the output data frame
            c(stateData[num,1], thisState)
            })
        ##------------------------------------------------
    } else if (outcome == "heart failure"){## outcome is heart failure
        ## apply the following anonymous function to each list element (dataframe for each state)
        ## and write to the output variable (enumerated list of states, with vector of hospital and
        ## state for each element)
        output <- lapply(stateOutcomes, function(stateData) {
            ## convert outcome column to numeric
            stateData[, 4] <- as.numeric(stateData[, 4]) 
            ## capture the state before we potentially strip out all the rows in the dataframe
            ## with the command after this one
            thisState <- stateData[1,2]
            ## strip out any hospitals with NA values for this outcome
            stateData <- subset (stateData, !is.na(stateData[, 4]))
            ## if looking for "worst", then we want the last entry
            if(num == "worst") {num <- nrow(stateData)}
            ## then sort by the outcome values, then hospital name
            stateData <- stateData[order(stateData[, 4], stateData[, 1]), ]
            ## add the selected (num) row to the output data frame
            c(stateData[num,1], thisState)
        })
        ##---------------------------------------------------
    } else { ## outcome is pneumonia
        ## apply the following anonymous function to each list element (dataframe for each state)
        ## and write to the output variable (enumerated list of states, with vector of hospital and
        ## state for each element)
        output <- lapply(stateOutcomes, function(stateData) {
            ## convert outcome column to numeric
            stateData[, 5] <- as.numeric(stateData[, 5]) 
            ## capture the state before we potentially strip out all the rows in the dataframe
            ## with the command after this one
            thisState <- stateData[1,2]
            ## strip out any hospitals with NA values for this outcome
            stateData <- subset (stateData, !is.na(stateData[, 5]))
            ## if looking for "worst", then we want the last entry
            if(num == "worst") {num <- nrow(stateData)}
            ## then sort by the outcome values, then hospital name
            stateData <- stateData[order(stateData[, 5], stateData[, 1]), ]
            ## add the selected (num) row to the output data frame
            c(stateData[num,1], thisState)
        })
    }
    ## convert the list into a data frame
    output <- do.call(rbind.data.frame, output)
    ## fix the column names of the output
    colnames(output) <- c("hospital", "state")
    ## print the output
    output
}