## Title: A Brief Analysis of Storm Risk for Urban Planners

## Synopsis: 

# Municipal managers need to make sound decisions about how to prioritize resources around severe weather events.  
# In order to do this effectively, storm managers should first understand what types of storms cause the most damage and death.
# This paper seeks to provide initial analysis using the last 25 years of available data from the NOAA Storm Database to 
# help guide decision-making at the urban level.  We show that only several specific storm types are the major cause of death
# in the United States.  However, to make the best decisions, storm managers should understand how their city's geography differentiates
# them from the average.  To do that, further analysis and study is required.

library(data.table)

## Data Processing

### Get the data
setwd('/Volumes/MacStorage/Coursera Data/storm/')
downloaded_filename <- "repdata-data-StormData.csv.bz2"
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", downloaded_filename)
### Load it in and show head and tail
data <- data.table(read.csv(downloaded_filename))
data
cleanData <- copy(data)
## 
data <- cleanData
## Results

# Look at fatalities as the key measure to health; though should probably include injuries in the report table and sum of percentage
# of injuries covered.
## Initial view
data[, .(dead = sum(FATALITIES)), by = EVTYPE][order(-dead)][1:20]
sum(data[, .(dead = sum(FATALITIES)), by = EVTYPE][order(-dead)][1:20]$dead)/sum(data$FATALITIES)
## Inconsistent coding example
unique(data[grepl("RIP CURRENT", EVTYPE)]$EVTYPE)
## Noted similar ways of coding similar events, so grep for similar and partially roll-up
data[, `:=` (EventTypeClean = as.character(EVTYPE),
             rollupComplete = 0)]
dt.grep <- data.table(grepString = c("TORN",   "(THUNDERSTORM|THUNDER|TSTM|THUNDEER) WIND","RIP CURRENT", "FLASH",      "COLD","SURF","HEAT","WIND","WINTER"),
                      eventClean = c("TORNADO","THUNDERSTORM WIND"                        ,"RIP CURRENT", "FLASH FLOOD","COLD","SURF","HEAT","WIND","WINTER"))
for(i in 1:nrow(dt.grep)) {
    data[rollupComplete == 0 & grepl(dt.grep[i]$grepString, EventTypeClean), 
         `:=` (EventTypeClean = dt.grep[i]$eventClean,
               rollupComplete = 1)]
}
## Grouped view; 96% complete, so done
data[, .(dead = sum(FATALITIES)), by = EventTypeClean][order(-dead)][1:20]
sum(data[, .(dead = sum(FATALITIES)), by = EventTypeClean][order(-dead)][1:20]$dead)/sum(data$FATALITIES)
### Clearly pay most attention to Tornados, heat and flooding (particularly flash flooding)

# Look at Property damage and Crop damage
## see if useful capturing of economic consequences
vec.validExponents <- c("K", "M", "B")
applyExponent <- function(x) { if(x == "K") { 1e3 } else if(x == "M") { 1e6 } else if(x == "B") { 1e9 } else { 0 } }
applyExponent <- function(x) {  }
data[, propertyDamage := as.numeric(0)]
data[PROPDMGEXP %in% vec.validExponents, propertyDamage := as.numeric(PROPDMG) * ifelse(PROPDMGEXP == "B", 1e9, ifelse(PROPDMGEXP == "M", 1e6, 1e3))]
data[, cropDamage := as.numeric(0)]
data[CROPDMGEXP %in% vec.validExponents, cropDamage := as.numeric(CROPDMG) * ifelse(PROPDMGEXP == "B", 1e9, ifelse(PROPDMGEXP == "M", 1e6, 1e3))]
data[, damage := propertyDamage + cropDamage]
data[, .(damage = sum(damage)), by = EventTypeClean][order(-damage)][1:20]
sum(data[, .(damage = sum(damage)), by = EventTypeClean][order(-damage)][1:20]$damage)/sum(data$damage)

# Cautious given high degree of concentration and therefore tail risk
data[, .(BGN_DATE, EVTYPE, damage, REMARKS)][order(-damage)][1:20]
sum(data[, .(BGN_DATE, EVTYPE, damage, REMARKS)][order(-damage)][1:20]$damage)/sum(data$damage)

## Make a CDF Plot to show skew of data!!

