source('downloadScript.R')

curr_locale <- Sys.getlocale("LC_TIME")

plot2 <- function(){
    Sys.setlocale("LC_TIME", "C")
    
    data <- load_data()
    
    data$Datetime <- strptime(paste(data$Date, data$Time), "%Y-%m-%d %H:%M:%S")
    data$Datetime <- as.POSIXct(data$Datetime)
    
    plot(data$Global_active_power ~ data$Datetime, type = "l",
         ylab = "Global Active Power (kilowatts)", xlab = "")
    dev.copy(png, file = "plot2.png")
    dev.off()
    
}

plot2()
Sys.setlocale("LC_TIME",curr_locale)