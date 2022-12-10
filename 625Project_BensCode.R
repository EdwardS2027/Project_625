## Initialize Workspace
setwd("C:/Users/IBHSm/Desktop/R Stuffs")
library(glmmfields)
library(glmmTMB)
library(spaMM)
library(simputation)
library(ggplot2)
library(lme4)
library(tidycensus)
library(stringr)
library(tidyverse)

## Read in the data
d.625 <- read.csv("625FinalProjectData.csv")
d.625 <- d.625[d.625$Group == "By Month", ]
d.625 <- d.625[d.625$State != "United States", ]
d.625 <- d.625[(d.625$Age.Group == "All Ages"), ]
d.625 <- d.625[, -c(1, 4)]
d.625$totMonth <- (d.625$Year - 2020) * 12 + d.625$Month

## Get population data for each state
d.pop <- read.csv("Population Data.csv")
d.pop <- d.pop[, -c(2,3)]
d.pop <- mutate(d.pop, across('Population.Estimate..July.1..2021..POP_2021.', str_replace, ',', ''))
d.pop <- mutate(d.pop, across('Population.Estimate..July.1..2021..POP_2021.', str_replace, ',', ''))

d.pop[, 2] <- as.numeric(d.pop[, 2])
d.pop[, 2] <- as.numeric(d.pop[, 2])

colnames(d.pop) <- c("region", "Population")
d.pop$region <- tolower(d.pop$region)

## Impute the data TESTING
## Nothing has been done with this so far
d.625.imp <- d.625
d.625.imp$Imp <- paste(d.625.imp$Condition.Group, d.625.imp$Age.Group)

d.625.imp <- impute_shd(d.625.imp, COVID.19.Deaths ~ totMonth | Imp)

d.625.agg <- aggregate(COVID.19.Deaths ~ d.625.imp$Imp, data = d.625.imp,FUN = mean)

## Testing some models

#ggplot(d.625, aes(x = totMonth, y = log(COVID.19.Deaths+1), group = State)) + geom_point(aes(color = State))

#model.1 <- glmer(COVID.19.Deaths ~ totMonth + Age.Group + Condition.Group + (1+totMonth|State), 
#      family = "poisson", data = d.625)

## Merge in population data
#d.625.new <- merge(d.625, d.pop, by = "State")

## Create Deaths per 100,000 variable
#d.625.new$COVID_Deaths_Per_100000 <- d.625.new$COVID.19.Deaths / (d.625.new$Population / 100000)


###### Map Stuff ##############################################################
## State Map data
states <- map_data("state")
d.625.new <- d.625[, -c(6, 7, 8, 9, 12)]
## Aggregate COVID-19 Deaths by date and State
d.625.new <- aggregate(COVID.19.Deaths ~ Start.Date + End.Date + Year + Month + State + totMonth, data = d.625.new, FUN = sum)

## Test with Jan 2021
#d.625.t <- d.625.new[d.625.new$totMonth == 13, ]
#d.625.t$COVID.19.Deaths[d.625.t$State == "New York"] <- d.625.t$COVID.19.Deaths[d.625.t$State == "New York"] + d.625.t$COVID.19.Deaths[d.625.t$State == "New York City"]

#d.625.t$region <- tolower(d.625.t$State)

## Combine New York and New York City
d.625.all <- d.625.new
d.625.all$COVID.19.Deaths[d.625.all$State == "New York"] <- d.625.all$COVID.19.Deaths[d.625.all$State == "New York"] + d.625.all$COVID.19.Deaths[d.625.all$State == "New York City"]
d.625.all$region <- tolower(d.625.all$State)

## Merge the COVID Data with the State Data for mapping
d.625.map <- merge(d.625.all, states, by = "region")

## Merge the COVID + Map Data with the population data
d.625.map <- merge(d.625.map, d.pop, by = "region")

## Create the COVID Deaths Per 100000 variable for plotting
d.625.map$COVID.Deaths.Per.100000 <- d.625.map$COVID.19.Deaths / (d.625.map$Population / 100000)

## Print all maps for each time point - 2 second delay between maps
for (i in 1:35) {
  print(ggplot(d.625.map[d.625.map$totMonth == i, ], aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = COVID.Deaths.Per.100000), color = "black") + scale_fill_gradient(low = "gray", high = "red", limits = c(0, 360)) + labs(title = paste("Number of Months:", as.character(i), sep = " "))) 
  Sys.sleep(2)
}

###### Looking at condition effects ############################################
## Create new dataset with relevant columns
d.625.c <- d.625[, -c(1:5, 7:9)]
d.625.c <- d.625.c[d.625.c$Condition.Group != "COVID-19", ]

## Aggregate COVID Deaths over date and Condition group
d.625.c <- aggregate(COVID.19.Deaths ~ totMonth + Condition.Group, data = d.625.c, FUN = sum)

## Plot time series of COVID Deaths
ggplot(d.625.c, aes(x = totMonth, y = COVID.19.Deaths, group = Condition.Group)) + geom_line(aes(color = Condition.Group), size = 0.75) + xlab("Months Since Dec 2019") + ylab("Deaths from COVID-19")

#j = 35
#print(ggplot(d.625.map[d.625.map$totMonth == j, ], aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = COVID.Deaths.Per.100000), color = "black") + scale_fill_gradient(low = "gray", high = "red", limits = c(0, 360)) + labs(title = paste("Months Since Jan 2020:", as.character(j-1), sep = " "))) 
