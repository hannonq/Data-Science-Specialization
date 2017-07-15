library(data.table)
library(lubridate)
library(dplyr)


load_data <- function(){
    if(!file.exists("./data.zip")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        message("Downloading File")
        download.file(fileUrl, destfile = './data.zip', method = "auto")    
        unzip('data.zip', exdir = "./")
    }
    data <- fread('household_power_consumption.txt', sep = ';', na.strings = '?')
    data$Date <- (dmy(data$Date))
    data <- filter(data, Date>= as.Date('2007-02-01') & Date<=as.Date('2007-02-02'))
    data
    
}
