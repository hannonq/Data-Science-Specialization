library(sqldf)
library(readr)

question2 <- function(){
    if(!file.exists("./data1.csv")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
        message("Downloading File")
        download.file(fileUrl, destfile = './data.csv', method = "auto")    
    }
    
    acs <- read.csv("./data.csv")
}

question3 <- function(){
    con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
    htmlCode <- readLines(con)
    close(con)
    htmlCode
}

question5 <- function(){
    x <- read_fwf(
            file="https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for",   
            skip=4,
            fwf_widths(c(12, 7, 4, 9, 4, 9, 4, 9, 4))
        )
    x
}

