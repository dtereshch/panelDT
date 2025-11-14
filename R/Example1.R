# Set up =======================================================================

library(AER)
library(tidyverse)


# Data =========================================================================

## Loading ---------------------------------------------------------------------

data(Fatalities)

Fatalities$fatalrate = Fatalities$fatal / Fatalities$pop * 10000


# Explore the data ==============================================================

## Explore the NAs --------------------------------------------------------------

list_incomplete(Fatalities, group = "state")
explore_incomplete(Fatalities, group = "state")

describe_participation(Fatalities, "state", "year")
plot_participation(Fatalities, "state", "year")

## Descriptive statistics -------------------------------------------------------

summary(Fatalities)
describe(Fatalities)
describe(Fatalities, c("fatal", "beertax"))

describe_by(Fatalities, c("fatal", "beertax"), "year")
describe_by(Fatalities, c("fatal", "beertax"), "state")

decompose_variation(Fatalities, c("fatal", "beertax"), "state")
decompose_variation(Fatalities, group = "state")


## Graphs ------------------------------------------------------------------------

plot_heterogeneity(Fatalities, fatalrate, year)
plot_heterogeneity(Fatalities, fatalrate, state, ylab = "Fatal Rate per 10,000")
