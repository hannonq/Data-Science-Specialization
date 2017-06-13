library(dplyr)
library(jpeg)
library(data.table)
library(Hmisc)


question1 <- function(){
    if(!file.exists("./data1.csv")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        message("Downloading File")
        download.file(fileUrl, destfile = './data1.csv', method = "auto")    
    }
    
    data <- read.csv("./data1.csv")
    agricultureLogical <- data$ACR==3 & data$AGS==6
    print(which(agricultureLogical))
}

question2 <- function(){
    myurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
    z <- tempfile()
    download.file(myurl,z,mode="wb")
    pic <- readJPEG(z, native = TRUE)
    file.remove(z) # cleanup
    
    print(quantile(pic, c(.30, .80)))
    

}

question3 <- function(){
    if(!file.exists("./gdp")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        message("Downloading File")
        download.file(fileUrl, destfile = './gdp', method = "auto")    
    }
    if(!file.exists("./educational")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
        message("Downloading File")
        download.file(fileUrl, destfile = './educational', method = "auto")    
    }
    gdp <- fread("./gdp", nrows = 190, skip = 4, select = c(1, 2, 4, 5), col.names=c("CountryCode", "Rank", "Economy", "Total"))
    educational <- fread("./educational", stringsAsFactors = F)

    merged = merge(gdp, educational, by = 'CountryCode')
    merged
}
    

