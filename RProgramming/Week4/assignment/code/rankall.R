rankall <- function(outcome, num="best"){
    
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
    
    #Replaces the string 'Not Available' with NA
    outcome <- data.frame(
        lapply(outcome, function(x){
            gsub("Not Available", NA, x)
        }
        )
    )
    outcome[, col_to_select] <- as.numeric(as.character(outcome[, col_to_select]))
    reduced <- outcome[c("State", "Hospital.Name", colnames(outcome[col_to_select]))]
    reduced <- reduced[complete.cases(reduced[, 3]),]
    
    sorted <- reduced[with(reduced, order(State, reduced[, 3], Hospital.Name)), ]
    state_split <- split(sorted, sorted$State)
    nth <- NULL
    result <- do.call(rbind, lapply(state_split, function(data){
        #Get nth in the rank
        if(num == "best"){
            nth <- 1
        }else if(num == "worst"){
            nth <- nrow(data)
        }else{
            nth <- num
        }
        data[nth, 1:2]
    }))
    colnames(result) <- c("state", "hospital")
    result
}
print(tail(rankall("heart failure"), 10))
#rankall("heart attack", 1)