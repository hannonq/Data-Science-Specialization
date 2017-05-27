pollutantmean <- function(directory, pollutant, id= 1:332){
    old_wd = getwd()
    setwd(directory)
    df <- data.frame()
    for(i in 1:length(id)){
        add <- read.csv(as.character(paste0(sprintf("%03d",id[i]),".csv")))
        df <- rbind(df,add)
    }
    setwd(old_wd)
    mean(df[[pollutant]], na.rm = TRUE)
}
#m  <- pollutantmean("specdata", "nitrate", 23)
#print(m)


