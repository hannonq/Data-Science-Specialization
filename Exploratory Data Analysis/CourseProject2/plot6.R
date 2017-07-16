library(dplyr)
library(ggplot2)

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Reproducing the code from plot 5 in order to plot both in the same graph
row <- 0
vehicle <- apply(SCC, 1, function(x){
    c <- grepl("vehicle|vessels|aircraft|onroad", x, ignore.case = TRUE)
    row <<- row + 1
    if(any(c))
        return (row)
    else
        return (NULL)
})
vehicle <- unlist(vehicle)
motor.vehicle <- SCC[vehicle, 1]
motor.vehicle.baltimore <- unique(filter(NEI, SCC %in% motor.vehicle & fips == "24510"))

motor.vehicle.year <- motor.vehicle.baltimore %>%
    group_by(year) %>%
    summarise(
        total = sum(Emissions),
        city = "baltimore"
    )
# End plot 5

# Begin plot 6
motor.vehicle.ca <- unique(filter(NEI, SCC %in% motor.vehicle & fips == "06037"))
motor.vehicle.year.ca <- motor.vehicle.ca %>%
    group_by(year) %>%
    summarise(
        total = sum(Emissions),
        city = "california"
    )

ba.ca <- rbind(motor.vehicle.year, motor.vehicle.year.ca)
g <- ggplot(data = ba.ca, aes(x = year, y = total, color = city)) + geom_line()
print(g)
dev.copy(png,'plot6')
dev.off()