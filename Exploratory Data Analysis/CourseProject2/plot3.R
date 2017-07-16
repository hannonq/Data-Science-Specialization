library(dplyr)
library(ggplot2)


NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

baltimoreType <- NEI %>%
    filter(fips == "24510") %>%
    group_by(type, year) %>%
    summarise(
        Emission = sum(Emissions)
    )

baltimoreType$type <- as.factor(baltimoreType$type)
q <- qplot(year, Emission, data = baltimoreType, facets = .~type) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_smooth(method = 'lm', se = FALSE)
print(q)

dev.copy(png,'plot3')
dev.off()