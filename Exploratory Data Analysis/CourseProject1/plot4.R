source('downloadScript.R')

curr_locale <- Sys.getlocale("LC_TIME")

plot4 <- function(){
    Sys.setlocale("LC_TIME", "C")
    
    data <- load_data()
    
    data$Datetime <- strptime(paste(data$Date, data$Time), "%Y-%m-%d %H:%M:%S")
    data$Datetime <- as.POSIXct(data$Datetime)
    
    par(mfrow = c(2, 2))
    plot(data$Global_active_power ~ data$Datetime, type = "l", ylab = "Global Active Power", 
         xlab = "")
    plot(data$Voltage ~ data$Datetime, type = "l", xlab="datetime")
    plot(data$Sub_metering_1 ~ data$Datetime, type = "l", ylab = "Energy sub metering", 
         xlab = "")
    lines(data$Sub_metering_2 ~ data$Datetime, col = "Red", type='l')
    lines(data$Sub_metering_3 ~ data$Datetime, col = "Blue", type='l')
    legend("topright", lty=1, col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           bty = "n", cex=.75)
    plot(data$Global_reactive_power ~ data$Datetime, type = "l", xlab="datetime")
    dev.copy(png, file = "plot4.png")
    dev.off()
}

plot4()
Sys.setlocale("LC_TIME",curr_locale)