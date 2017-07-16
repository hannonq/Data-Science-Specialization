library(dplyr)
library(ggplot2)
library(reshape2)

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

NEIyear <- NEI %>%
    group_by(year) %>%
    summarise(
        Total.Emissions = sum(Emissions)
    )

# plot(NEIyear)

baltimore <- NEI %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(
        Total.Emissions = sum(Emissions)
    )

# plot(baltimore)

baltimoreType <- NEI %>%
    filter(fips == "24510") %>%
    group_by(type, year) %>%
    summarise(
        Emission = sum(Emissions)
    )


baltimoreType$type <- as.factor(baltimoreType$type)
# print(str(baltimoreType))
q <- qplot(year, Emission, data = baltimoreType, facets = .~type) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_smooth(method = 'lm', se = FALSE)
print(q)

# Item 4
# Finding coal combustion-related words:
x <- sapply(SCC, function(x) grepl("coal", x, ignore.case = TRUE))
# print(unique(SCC[x]))

# Combustion-related words
combustion <- tolower(c("Synfuel", "Fuel", "Combustion", "Hand-fired", "Stoker", "Furnace", "Vessels", "Kiln"))

# Subset sources containing the word 'coal' and any of the words in the 'combustion' vector
row <- 0
coal.combustion <- apply(SCC, 1, function(x){
    # Find all with 'coal' word
    c <- grepl("coal", x, ignore.case = TRUE)
    comb <- FALSE

    if(any(c)){
        comb <- grepl(paste(combustion, collapse="|"), x, ignore.case = TRUE)
    }
    row <<- row + 1
    if(any(comb))
        return (row)
    else
        return (NULL)

})
coal.combustion <- unlist(coal.combustion)
coal.combustion.scc <- SCC[coal.combustion, 1]
coal.emissions <- unique(filter(NEI, SCC %in% coal.combustion.scc))

coal.emissions.year <- coal.emissions %>%
    group_by(year) %>%
    summarise(
        total = sum(Emissions)
    )

print(qplot(year, total, data = coal.emissions.year) + geom_line())


# Item 5
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

print(qplot(year, total, data = motor.vehicle.year) + geom_line())

motor.vehicle.ca <- unique(filter(NEI, SCC %in% motor.vehicle & fips == "06037"))
motor.vehicle.year.ca <- motor.vehicle.ca %>%
    group_by(year) %>%
    summarise(
        total = sum(Emissions),
        city = "california"
    )

ba.ca <- rbind(motor.vehicle.year, motor.vehicle.year.ca)
print(ba.ca)
g <- ggplot(data = ba.ca, aes(x = year, y = total, color = city)) + geom_line()
print(g)



