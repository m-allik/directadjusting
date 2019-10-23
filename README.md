

# directadjusting


This is a and altered version of WetRobot/directadjusting. It is a start of a wrapper function to:

1. deal with age groups such as 15-16 and 17-19 provided in the data - that would have to be a labelled factor variable to work! e.g. with labels such as "15-16" to clearly spell out the age.
2. give user the option to ooutput direct asjusted rates for a specific age group such as 10-16 and 17-24 etc.

Idea is that the wrapper will prep data to then use  direct_adjusted_estimates

The goal of directadjusting is to provide an easy and fast method of computing
direct adjusted estimates such as age-adjusted estimates.

## Installation

``` r
devtools::install_github("WetRobot/directadjusting")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library("directadjusting")

# suppose we have poisson rates that we want to adjust for by age group.
# they are stratified by sex.
library("data.table")
set.seed(1337)

offsets <- rnorm(8, mean = 1000, sd = 100)
baseline <- 100
sex_hrs <- rep(1:2, each = 4)
age_group_hrs <- rep(c(0.75, 0.90, 1.10, 1.25), times = 2)
counts <- rpois(8, baseline * sex_hrs * age_group_hrs)

# raw estimates
my_stats <- data.table(
  sex = rep(1:2, each = 4),
  ag = rep(1:4, times = 2),
  e = counts / offsets
)
my_stats[["v"]] <- my_stats[["e"]] / offsets

# adjusted by age group
my_adj_stats <- direct_adjusted_estimates(
  stats_dt = my_stats,
  stat_col_nms = "e",
  var_col_nms = "v",
  conf_lvls = 0.95,
  conf_methods = "log",
  stratum_col_nms = "sex",
  adjust_col_nms = "ag",
  weights = c(200, 300, 400, 100)
)


```

