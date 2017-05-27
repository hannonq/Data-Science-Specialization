library(dplyr)

complete <- function(directory, id=1:332){
    old_wd = getwd()
    setwd(directory)
    df <- data.frame()
    for(i in length(id):1){
        add <- read.csv(as.character(paste0(sprintf("%03d",id[i]),".csv")))
        df <- rbind(df,add)
    }
    setwd(old_wd)
    
    new_df <- na.omit(df)
    nobs <- tally(group_by(new_df, ID), sort = TRUE)
    answer <- data.frame(nobs)
    colnames(answer) <- c("id", "nobs")
    answer
}

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

#df <- complete("specdata", 1)
#print(df)