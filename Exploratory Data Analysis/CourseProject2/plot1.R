library(dplyr)

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

NEIyear <- NEI %>%
    group_by(year) %>%
    summarise(
        Total.Emissions = sum(Emissions)
    )

plot(NEIyear, type='l')
title(main = "PM2.5 in the US")
dev.copy(png,'plot1')
dev.off()