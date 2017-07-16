library(dplyr)
library(ggplot2)

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Finding coal combustion-related words:
x <- sapply(SCC, function(x) grepl("coal", x, ignore.case = TRUE))

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

q <- qplot(year, total, data = coal.emissions.year) +
    geom_line() +
    labs(title="Emissions from coal combustion-related sources")
print(q)
dev.copy(png,'plot4')
dev.off()