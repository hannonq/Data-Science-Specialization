library(data.table)
source('downloadScript.R')

plot1 <- function(){
    data <- load_data()
    hist(data$Global_active_power, 
         main = 'Global Active Power',
         xlab = 'Global Active Power (kilowatts)',
         col = 'red')
    dev.copy(png, 'plot1.png')
    dev.off()
}

plot1()
