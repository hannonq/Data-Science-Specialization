library(data.table)


question1 <- function(){
    if(!file.exists("./data1.csv")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        message("Downloading File")
        download.file(fileUrl, destfile = './data1.csv', method = "auto")    
    }
    
    data <- read.csv("./data1.csv")
    
    split_list <- lapply(names(data), function(x) strsplit(x, "wgtp"))
    print(split_list[123])
}

question2 <- function(){
    if(!file.exists("./data2.csv")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        message("Downloading File")
        download.file(fileUrl, destfile = './data2.csv', method = "auto")    
    }
    
    gdp <- fread("./data2.csv", nrows = 190, skip = 4, select = c(1, 2, 4, 5), col.names=c("CountryCode", "Rank", "Economy", "Total"))
    x <- sapply(gdp$Total, function(x){
        no_comma <- as.numeric(gsub(",", "", x))
    })
    print(mean(x))
    gdp
}

question4 <- function(){
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

#question4()