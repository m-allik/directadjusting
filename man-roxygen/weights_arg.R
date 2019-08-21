




#' @md
#' @param weights `[double, data.table, character]`
#'
#' the weights need not sum to one as this is ensured internally; you may
#' supply weights in one of the following ways:
#'
#' - `double`: a vector of weights can be given when adjusting by exactly
#'   one variable (e.g. age group); the length must match the number of unique
#'   values in the adjusting variable
#' - `data.table`: with one or more columns with names matching to those
#'   variables that are used to adjust estimates, and one column named
#'   `weight`; e.g. `data.table(agegroup = 1:3, weight = c(100, 500, 400))`
#' - `character`: a string specifying one of the weighting schemas integrated
#'   into this package; see \code{\link{integrated_weight_schemas}}; again
#'   this is only allowed when you are adjusting by exactly one variable




