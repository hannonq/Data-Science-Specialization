rankhospital <- function(state, outcome, num = "best"){
    
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

    #[, c("Hospital.Name", names(sorted[, col_to_select]))]
    #print(tail(sorted[c("Hospital.Name", colnames(sorted[col_to_select]))]))
    
    #Get nth in the rank
    nth <- NULL
    if(num == "best"){
        nth <- 1
    }else if(num == "worst"){
        nth <- nrow(sorted)
    }else{
        nth <- num
    }
    
    as.character(sorted$Hospital.Name[nth])
}

# print(rankhospital("TX", "heart failure", 4))
# print(rankhospital("MD", "heart attack", "worst"))