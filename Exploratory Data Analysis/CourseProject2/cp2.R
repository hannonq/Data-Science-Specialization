library(dplyr)
library(ggplot2)
library(reshape2)

# NEI <- readRDS("data/summarySCC_PM25.rds")
# SCC <- readRDS("data/Source_Classification_Code.rds")

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


# View(SCC)
        




