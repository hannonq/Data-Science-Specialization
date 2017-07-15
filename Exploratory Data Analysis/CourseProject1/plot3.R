source('downloadScript.R')

curr_locale <- Sys.getlocale("LC_TIME")

plot3 <- function(){
    Sys.setlocale("LC_TIME", "C")
    
    data <- load_data()
    
    data$Datetime <- strptime(paste(data$Date, data$Time), "%Y-%m-%d %H:%M:%S")
    data$Datetime <- as.POSIXct(data$Datetime)
    
    plot(data$Sub_metering_1 ~ data$Datetime, type = "l", 
         ylab = "Energy sub metering", xlab = "")
    lines(data$Sub_metering_2 ~ data$Datetime, col = "Red")
    lines(data$Sub_metering_3 ~ data$Datetime, col = "Blue")
    legend("topright", lty = 1, col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    dev.copy(png, file = "plot3.png")
    dev.off()
    
}

plot3()
Sys.setlocale("LC_TIME",curr_locale)