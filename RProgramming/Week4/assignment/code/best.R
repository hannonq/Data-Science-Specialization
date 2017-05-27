best <- function(state, outcome){
    
    #Selects the right column according to the outcome
    col_to_select <- NULL
    if(outcome == "heart attack"){
        col_to_select <- 11
    }else if(outcome == "heart failure"){
        col_to_select <- 17
    }else if(outcome == "pneumonia"){
        col_to_select <- 23
    }else{
        stop("invalid outcome")
    }
    
    #Reads CSV
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #Checks state
    if(!(state %in% outcome$State)){
        stop("invalid state")
    }
    
    #Replaces the string 'Not Available' with NA
    outcome <- data.frame(
        lapply(outcome, function(x){
                    gsub("Not Available", NA, x)
                }
            )
        )
    #Selects the given state
    outcome_by_state <- outcome[which(outcome$State == state), ]
    
    #Converts the rate to numeric format
    outcome_by_state[, col_to_select] <- as.numeric(as.character(outcome_by_state[, col_to_select]))
    
    #Sorts by smallest rate and then by name
    sorted <- na.omit(outcome_by_state[with(outcome_by_state, order(outcome_by_state[, col_to_select], Hospital.Name)), ])
    
    #returns the result
    as.character(sorted$Hospital.Name[1])
}

# print(best("TX", "heart attack"))
# print(best("TX", "heart failure"))
# print(best("MD", "heart attack"))
# print(best("MD", "pneumonia"))