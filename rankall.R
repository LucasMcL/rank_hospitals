rankall <- function(outcome, num = "best"){
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data[ ,11] <- as.numeric(data[ ,11])
        data[ ,17] <- as.numeric(data[ ,17])
        data[ ,23] <- as.numeric(data[ ,23])
        
        
        ## Check that outcome is valid and define alph.states for later use
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        alph.states <- sort(c(state.abb, "GU", "PR", "VI", "DC"))
        if(is.element(outcome, valid.outcomes) == FALSE) stop("invalid outcome")
        column <- column_number(outcome)
        hospital <- character(54)
        
        ## For each state, find the hospital of the given rank
        ## First, order the data by state and mortality rate
        ## Then, remove rows with NAs in the mortalit rate column
        ## State index contains number of obs. for each state for given outcome
        cleaned.data <- data[complete.cases(data[, column]), ]
        ordered.data <- cleaned.data[order(cleaned.data[ ,7], cleaned.data[ ,column], cleaned.data[ ,2]), ]
        state.index <- index(alph.states, ordered.data)
        
        
        ## If best, use state.index info to go to first obs for each state
        ## Record hospital name in hospital character vector
        if(num == "best"){
                row <- 1
                for(i in 1:54){
                        hospital[i] <- ordered.data[row, 2]
                        row <- row + state.index[i]
                }
        }
        ## If worst, use state.index info to go to last obs for each state
        ## Record hospital name in hospital character vector
        else if (num == "worst"){
                row <- 0
                for(i in 1:54){
                        row <- row + state.index[i]
                        hospital[i] <- ordered.data[row, 2]
                }
        }
        ## If num, first check to see if there are as many obs as value of num
        ## If so, record hospital name for that rank
        ## If not, record NA in the character vector
        else{
                row <- 0
                for(i in 1:54){
                        if(num <= state.index[i]){
                                hospital[i] <- ordered.data[row + num, 2]
                        }
                        else hospital[i] <- NA
                        row <- row + state.index[i]
                }
        }
        
        
        ##Return a data frame with the hospital names and the appreviated state name
        data.frame(hospital = hospital, state = alph.states, stringsAsFactors = FALSE)
}

column_number <- function(outcome){
        if(outcome == "heart attack") 11
        else if(outcome == "heart failure") 17
        else 23
}

index <- function(alph.states, data){
        index <- integer(54)
        for(i in 1:54){
                index[i] <- length(which(data[ ,7] == alph.states[i]))
        }
        index
}