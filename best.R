best <- function(state, outcome){
        ## Read in data as characters and convert appropriate columns to doubles
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data[ ,11] <- as.numeric(data[ ,11])
        data[ ,17] <- as.numeric(data[ ,17])
        data[ ,23] <- as.numeric(data[ ,23])
        
        ## Check that state and outcome are valid
        ## Valid outcomes are "heart attack", "heart failure" and "pneumonia"
        ## Valid states are the 50 U.S. states plus abbreviations for Guam,
        ## Puerto Rico, Virgin Islands, and Washington DC
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        valid.states <- c(state.abb, "GU", "PR", "VI", "DC")
        if(is.element(state, valid.states) == FALSE) stop("invalid state")
        if(is.element(outcome, valid.outcomes) == FALSE) stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death rate
        ## Initialize variables.  Column is based on what outcome you're looking at
        ## lowest.rate is the current lowest mortality rate
        ## hospital.name is the name of the hospital to be output
        ## state.rows is an integer vector containing the rows applying to the given state
        column <- column_number(outcome)
        hospital.name <- character(0)
        lowest.rate <- 100
        state.rows <- which(data[,7] == state)
        
        ## Loop through each data for the given state and outcome
        ## Assign a new lowest.rate if that data is lower than previously found
        ## Check for alphabetical order if data is equal to lowest.rate
        for(i in seq(state.rows)){
                row <- state.rows[i]
                if(is.na(data[row,column])) next
                if(data[row,column] < lowest.rate){
                        lowest.rate <- data[row, column]
                        hospital.name <- data[row, 2]
                }
                else if(data[row,column] == lowest.rate){
                        if(data[row, 2] < hospital.name){
                                hospital.name <- data[row, 2]
                        }
                }
        }
        
        hospital.name
}

column_number <- function(outcome){
        if(outcome == "heart attack") 11
        else if(outcome == "heart failure") 17
        else 23
}