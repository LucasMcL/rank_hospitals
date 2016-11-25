rankhospital <- function(state, outcome, num = "best"){
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
        
        ## Return hospital name in that state with the given rank
        ## Initialize hospital.names vector and mortality.rate vector
        ## These will be filled with appropriate values for given state
        ## and used to create a data frame later
        column <- column_number(outcome)
        state.rows <- which(data[,7] == state)
        hospital.name <- character(length(state.rows))
        mortality.rate <- numeric(length(state.rows))
        
        
        for(i in seq(state.rows)){
                row <- state.rows[i]
                hospital.name[i] <- data[row,2]
                mortality.rate[i] <- data[row,column]
        }
        
        ## Creates data frame with variables hospital.name and mortality.rate
        ## Orders first by mortality rate, then by alphabetical order
        ## Removes any rows containing NAs
        state.data <- data.frame(hospital.name = hospital.name, mortality.rate = mortality.rate, stringsAsFactors = FALSE)
        ordered.data <- state.data[order(state.data$mortality.rate, state.data$hospital.name),]
        ordered.data <- na.omit(ordered.data)
        n <- nrow(ordered.data)
        
        ##Returns hospital name as character vector
        if(num == "best") ordered.data[1,1]
        else if(num == "worst") ordered.data[n, 1]
        else if(num > n) NA
        else ordered.data[num, 1]
}

column_number <- function(outcome){
        if(outcome == "heart attack") 11
        else if(outcome == "heart failure") 17
        else 23
}