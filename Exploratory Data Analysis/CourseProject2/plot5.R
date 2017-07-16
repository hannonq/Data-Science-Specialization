library(dplyr)
library(ggplot2)

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

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

q <- qplot(year, total, data = motor.vehicle.year) + 
    geom_line() +
    labs(title="Emissions from motor vehicle sources in Baltimore City")
print(q)
dev.copy(png,'plot5')
dev.off()