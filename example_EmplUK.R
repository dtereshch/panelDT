# Example
# panelDT functions usage with plm::EmplUK dataset

# Setup ========================================================================================

library(devtools)
install_github("dtereshch/panelDT")

library(plm)
library(fixest)
library(panelDT)


# Load & prepare the data =======================================================================

data(EmplUK)
is.pbalanced(EmplUK)

df <- make.pbalanced(EmplUK, balance.type = "fill")

df_plm <- pdata.frame(df, index = c("firm", "year"))
df_fixest <- panel(df, panel.id = ~ firm + year)

class(df)
class(df_plm)
class(df_fixest)


# Explore the data ==============================================================================

## General case ---------------------------------------------------------------------------------

### Explore NAs
find_incomplete(df, group = "firm")
explore_incomplete(df, group = "firm")

describe_participation(df, group = "firm", time = "year")
explore_participation(df, group = "firm", time = "year")
plot_participation(df, group = "firm", time = "year")

### Descriptive statistics
describe(df)
describe(df, variables = c("emp", "wage", "capital", "output"))

describe_by(
  df,
  variables = c("emp", "wage", "capital", "output"),
  group = "year"
)

describe_by(
  df,
  variables = c("emp", "wage", "capital", "output"),
  group = "firm"
)

decompose_variation(
  df,
  variables = c("emp", "wage", "capital", "output"),
  group = "firm"
)

decompose_variation(df, group = "firm")

### Plot heterogeneity

plot_heterogeneity(df, variable = "emp", group = "year")
plot_heterogeneity(df, variable = "emp", group = "firm")

plot_heterogeneity(df, variable = "emp", group = c("firm", "year"))

## plm::pdata.frame() class ---------------------------------------------------------------------

## fixest::panel() class ------------------------------------------------------------------------
