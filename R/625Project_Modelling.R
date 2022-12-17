###### Initialize Workspace ###################################################
setwd("C:/Users/IBHSm/Desktop/R Stuffs")
library(ggplot2)
library(tidyverse)
library(splines)
library(MASS)

###### Read in the data #######################################################
d.625 <- read.csv("625FinalProjectData.csv")
d.625 <- d.625[d.625$Group == "By Month", ] # Examine the data monthly rather than by year or total
d.625 <- d.625[d.625$State != "United States", ] # Interested in each state individually, not the total for the US
d.625 <- d.625[(d.625$Age.Group != "All Ages") & (d.625$Age.Group != "Not stated"), ] # Remove aggregate and not stated groups for Age
d.625$totMonth <- (d.625$Year - 2020) * 12 + d.625$Month # Create numerical variable for number of months since Dec 2019 for model fitting
d.625 <- d.625[, -c(1:6, 10, 13, 14)] # Remove unnecessary columns (time cols, Group, ICD10 Codes, Flag, etc.)

###### IMPUTATION #############################################################
#### Impute each missing point from uniform(1,9)
## Copy the dataset for imputation
d.625.imp <- d.625

## Impute from uniform(1,9)
set.seed(123456)
d.625.imp$COVID.19.Deaths[is.na(d.625.imp$COVID.19.Deaths)] = round(runif(length(d.625.imp$COVID.19.Deaths[is.na(d.625.imp$COVID.19.Deaths)]), 0.5, 9.49))

###### Regression Modeling ####################################################
## Copy the dataset for modeling
d.625.m <- d.625.imp

## Aggregate New York and New York City
d.625.m$State[d.625.m$State == "New York City"] = "New York"
d.625.m <- aggregate(COVID.19.Deaths ~ State + Condition.Group + Condition + Age.Group + totMonth, data = d.625.m, FUN = sum)

## Remove COVID-19 deaths without co-existing conditions, as well as the All category
d.625.m <- d.625.m[(d.625.m$Condition.Group != "COVID-19") & (d.625.m$Condition.Group != "All other conditions and causes (residual)"), ]

#### Exploring a few thinga
### Creating Spatial dataframe
## Read in the ggplot2 states data
#states <- raster::getData("GADM", country = "United States", level = 1)

## TESTING WITH SUBSET OF DATA
#d.625.TEST <- d.625.m[(d.625.m$totMonth == 5) & (d.625.m$Age.Group == "85+") & (d.625.m$Condition.Group == "Respiratory diseases"), ]

#d.625.sp.TEST <- sp::merge(states, d.625.TEST, by.x = "NAME_1", by.y = "State", duplicateGeoms = TRUE)

#nb <- poly2nb(d.625.sp.TEST)

## Creating a list of polygons for the states
#d.625.sp <- sp::merge(states, d.625.m, by.x = "NAME_1", by.y = "State", duplicateGeoms = TRUE)

#### It becomes apparent that the spatial modelling is beyond the scope of this project
#### We will instead fit a number of other models, including:

## Model with and without log link (lm vs glm w/ poisson response)
## Depending on overdispersion, explore negative binomial model
## Model with and without spline for time
## Explore using GEE / GLMM for 

###### Modelling several potential models #####################################
## Naive model - lm (Normal outcome) with linear time

m.naive <- lm(COVID.19.Deaths ~ totMonth + Condition.Group + Age.Group + State, data = d.625.m)
summary(m.naive)
# Adjusted R^2 = 0.1269 - Fits well enough, but we know this model to not be very accurate

## lm with a linear spline
# Spline knots obtained from exploratory analysis with time series and condition groups,
m.lm.spline <- lm(COVID.19.Deaths ~ bs(totMonth, degree = 1, knots = c(4, 9, 13, 18, 21, 23, 25)) + Condition.Group + Age.Group + State, data = d.625.m)
summary(m.lm.spline)

## glm with poisson link - linear spline
m.glm.spline <- glm(COVID.19.Deaths ~ bs(totMonth, degree = 1, knots = c(4, 9, 13, 18, 21, 23, 25)) + Condition.Group + Age.Group + State, data = d.625.m, family = "poisson")
summary(m.glm.spline)
# Check for overdispersion? Negative Binomial model may be more approbriate
pchisq(m.glm.spline$deviance, df=m.glm.spline$df.residual, lower.tail=FALSE)
# p-value = 0, reject the null - There is overdispersion

## glm with Negative Binomial link, linear spline for time
m.nb.spline <- glm.nb(COVID.19.Deaths ~ bs(totMonth, degree = 1, knots = c(4, 9, 13, 18, 21, 23, 25)) + Condition.Group + Age.Group + State, data = d.625.m)
summary(m.nb.spline)
# Better fit - Dispersion parameter for Negative Binomial (0.6468) family taken to be 1

## Using GEE with negative binomial link, linear spline for time?
# m.nb.gee <- geem(COVID.19.Deaths ~ bs(totMonth, degree = 1, knots = c(4, 9, 13, 18, 21, 23, 25)) + Condition.Group + Age.Group, id = State, data = d.625.m, corstr = "exchangeable", family = negative.binomial(1))
# Unable to fit

## We can then consider the glm with a Negative Binomial link and a linear spline for time to be the final model.
