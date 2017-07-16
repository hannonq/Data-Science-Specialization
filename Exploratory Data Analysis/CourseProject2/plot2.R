library(dplyr)

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

baltimore <- NEI %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(
        Total.Emissions = sum(Emissions)
    )

plot(baltimore, type="l")
title(main = "PM2.5 in Baltimore City")
dev.copy(png,'plot2')
dev.off()