library(xlsx)
library(XML)
library(data.table)

quiz1.question1 <- function(){
    
    if(!file.exists("./data1.csv")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        message("Downloading File")
        download.file(fileUrl, destfile = './data.csv', method = "auto")    
    }
    
    dataFrame <- read.csv("./data.csv")
    print(sum(dataFrame$VAL==24, na.rm=TRUE))
}

quiz1.question3 <- function(){
    
    if(!file.exists("./data2.xlsx")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
        message("Downloading File")
        download.file(fileUrl, destfile = './data2.xlsx', method = "auto")
    }
    colIndex <- 7:15
    rowIndex <- 18:23
    dat <- read.xlsx("data2.xlsx", sheetIndex=1, header=TRUE,
                     colIndex = colIndex, rowIndex = rowIndex)
    print(sum(dat$Zip*dat$Ext,na.rm=T))
}

quiz1.question4 <- function(){
    
    fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"    
    message("Downloading file")
    doc <- xmlTreeParse(fileUrl, useInternal=TRUE)
    rootNode <- xmlRoot(doc)

    zipList <- xpathSApply(rootNode, "//zipcode", xmlValue)
    print(sum(zipList == "21231"))

}

quiz1.question5 <- function(){
    if(!file.exists("./data3.csv")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
        message("Downloading File")
        download.file(fileUrl, destfile = './data3.csv', method = "auto")    
    }
    
    #DT <- fread("data3.csv")
    #print(head(DT))
    
}

quiz1.question5()