
#' @md
#' @title Wrapper function for direct asjusting
#' @description Compute direct adjusted estimates from a table of statistics.
#' @param data Data set name
#' @param outcome
#' @param population
#' @param age
#' 
#' 
#'
#' # Mock data
#' data  <- st_population
#'
#' ds_rates(data, counts, pop, age, sex, age_groups = c("0-9", "10-16", "17-24"))
#' 
#' 
#' 
#' 
#' 

# function to start here
# commented out stuff for now

#ds_rates <- function(data, outcome, population, age, sex, age_group) {
  

# old subset function
#groups = NULL
#df <- subset_q(data, substitute(groups), substitute(c(outcome, population, age, sex))) # select data from data frame

# load my_data and sysdata!!
# then follow code

library("directadjusting")
library("data.table")

load("R/sysdata.rda")
load("data/my_data.rda")

df <- my_data
age_groups = c("0-9", "10-16", "17-24")

names(df) <- c("outcome", "population", "age", "sex") # Give names to use within function
df$age_groups <- as.numeric(df$age)

# Extract age groups from the data
age_levels <- levels(df$age) # extract levels
age_levels <- strsplit(age_levels, split = "[^0-9]") # extract numbers and place them in a list

# Check for open age groups and close those with age 99
open_ag <- which(lapply(age_levels, length) == 1)
age_levels[[open_ag]][2] <- "99"

age_list <- lapply(age_levels, function(x) do.call(":", list(x[1], x[2]))) # provide a list with all ages included

# Extract output age groups
age_groups_out <- strsplit(age_groups, split = "[^0-9]") # extract numbers and place them in a list
age_groups_out <- lapply(age_groups_out, function(x) do.call(":", list(x[1], x[2]))) # provide a list with all ages included


n_ag <- length(age_list) # number of age groups in data
n_ag_out <- length(age_groups_out) # number of output age groups


st_population$age_groups <- NA # add age grouos to to weights 
st_population$output_groups <- NA # add output age groups to weights

for (i in 1:n_ag) { # code output age groups
  
  st_population$age_groups[st_population$age %in% age_list[[i]]] <- i
  
}

for (i in 1:n_ag_out) { # code output age groups
  
  st_population$output_groups[st_population$age %in% age_groups_out[[i]]] <- i
  
}

st_population[, weight_numerator := sum(esp2013_21), by = list(unique.values = age_groups)]
# st_population[, weight_denominator := sum(esp2013_21), by = list(unique.values = output_groups)]

# Slim data down
st_population <- st_population[, list(age_groups, output_groups, weight_numerator)]
#setkeyv(st_population, c('age_groups','output_groups'))
age_strata <- unique(st_population)

# merge to data
data <- merge(df, age_strata, by = "age_groups")
data$crude_rate <- data$outcome/data$population

weights_data <- age_strata$weight_numerator
data <- data.table(data)


 
my_adj_stats <- direct_adjusted_estimates(
  stats_dt = data,
  stat_col_nms = "crude_rate",
  var_col_nms = NULL,
  conf_lvls = 0.95,
  conf_methods = "log",
  stratum_col_nms = "sex",
  adjust_col_nms = "output_groups",
  weights = "weight_numerator")


# Error in weightschemastring_to_weights(weights) : TODO
#output(my_adj_stats)

#}