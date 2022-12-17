## Initialize Workspace
setwd("C:/Users/IBHSm/Desktop/R Stuffs")
library(tidyverse)
library(ggplot2)
library(stringr)

## Read in the data
d.625 <- read.csv("625FinalProjectData.csv")
d.625 <- d.625[d.625$Group == "By Month", ]
d.625 <- d.625[d.625$State != "United States", ]
d.625$totMonth <- (d.625$Year - 2020) * 12 + d.625$Month

###### POPULATION DATA FOR MAPPING RATES ######################################
## Get population data for each state
d.pop <- read.csv("Population Data.csv")
d.pop <- d.pop[, -c(2,3)]
d.pop <- mutate(d.pop, across('Population.Estimate..July.1..2021..POP_2021.', str_replace, ',', ''))
d.pop <- mutate(d.pop, across('Population.Estimate..July.1..2021..POP_2021.', str_replace, ',', ''))
# Necessary to do twice or else it will only remove one comma, and the following command will not be able to coerce it to a numerical variable.
d.pop[, 2] <- as.numeric(d.pop[, 2])

## Manipulate columns to merge neatly with ggplot state data
colnames(d.pop) <- c("region", "Population")
d.pop$region <- tolower(d.pop$region)

###### Mapping - Spatial analysis #############################################
## Mapping data
states <- map_data("state")
d.625.mapping <- d.625[d.625$Age.Group == "All Ages", -c(1:6, 10, 13, 14)]
d.625.mapping <- aggregate(COVID.19.Deaths ~ totMonth + State, data = d.625.mapping, FUN = sum)

## Aggregate New York and New York City
d.625.mapping$State[d.625.mapping$State == "New York City"] = "New York"
d.625.mapping <- aggregate(COVID.19.Deaths ~ State + totMonth, data = d.625.mapping, FUN = sum)

## Merge Datasets (population and states data)
d.625.mapping$region <- tolower(d.625.mapping$State)

d.625.mapping <- merge(d.625.mapping, states, by = "region")
d.625.mapping <- merge(d.625.mapping, d.pop, by = "region")

## Create the COVID Deaths Per 100000 variable for plotting
d.625.mapping$COVID.Deaths.Per.100000 <- d.625.mapping$COVID.19.Deaths / (d.625.mapping$Population / 100000)

## Print all maps for each time point - 2 second delay between maps
for (i in 1:35) {
  print(ggplot(d.625.mapping[d.625.mapping$totMonth == i, ], aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = COVID.Deaths.Per.100000), color = "black") + scale_fill_gradient(low = "gray", high = "red", limits = c(0, 360)) + labs(title = paste("Number of Months:", as.character(i), sep = " "))) 
  Sys.sleep(2)
}

###### Time series for condition group - Temporal and conditions analysis #####
## Subset the data for time series analysis
d.625.c <- d.625[d.625$Condition.Group != "COVID-19", ]

## Aggregate COVID Deaths over date and Condition group
d.625.c <- aggregate(COVID.19.Deaths ~ totMonth + Condition.Group, data = d.625.c, FUN = sum)

## Plot time series of COVID Deaths
ggplot(d.625.c, aes(x = totMonth, y = COVID.19.Deaths, group = Condition.Group)) + geom_line(aes(color = Condition.Group), size = 0.75) + xlab("Months Since Dec 2019") + ylab("Deaths from COVID-19")
