




#' @md
#' @title Direct Adjusted Estimates
#' @description Compute direct adjusted estimates from a table of statistics.
#' @param stats_dt `[data.table]` (mandatory, no default)
#'
#' a `data.table` containing estimates and variance estimates of statistics
#' @param specs_dt `[data.table]` (mandatory, no default)
#'
#' a `data.table` specifying names of columns in `stats_dt` for the estimates
#' (in column `est`) and their variance estimates (in column `var`);
#' all columns of `stats_dt` specified in `specs_dt$est` are always adjusted, and
#' those which have corresponding non-`NA` values in `specs_dt$var` will also
#' have weighted variances computed, and from these the appropriate confidence
#' intervals; the confidence levels should be given in `specs_dt$conf_lvl` and
#' the method of computing the intervals in `specs_dt$conf_method`;
#' see \code{\link{confidence_interval_methods}} for supported methods;
#' see also **Examples**
#' @param stratum_col_nms `[NULL, character]` (optional, default `NULL`)
#'
#' names of columns in `stats_dt` by which statistics are stratified (and they
#' should be stratified by these columns after direct adjusting)
#' @param adjust_col_nms `[character]` (mandatory, no default)
#'
#' names of columns in `stats_dt` by which statistics are currently stratified
#' and by which the statistics should be adjusted
#' @template weights_arg
#'
#' @examples
#'
#' # suppose we have poisson rates that we want to adjust for by age group.
#' # they are stratified by sex.
#' library("data.table")
#' set.seed(1337)
#'
#' offsets <- rnorm(8, mean = 1000, sd = 100)
#' baseline <- 100
#' sex_hrs <- rep(1:2, each = 4)
#' age_group_hrs <- rep(c(0.75, 0.90, 1.10, 1.25), times = 2)
#' counts <- rpois(8, baseline * sex_hrs * age_group_hrs)
#'
#' # raw estimates
#' my_stats <- data.table(
#'   sex = rep(1:2, each = 4),
#'   ag = rep(1:4, times = 2),
#'   e = counts / offsets
#' )
#' my_stats[["v"]] <- my_stats[["e"]] / offsets
#'
#' # adjusted by age group
#' my_adj_stats <- direct_adjusted_esimates(
#'   stats_dt = my_stats,
#'   specs_dt = data.table(est = "e", var = "v", conf_lvl = 0.95),
#'   stratum_col_nms = "sex",
#'   adjust_col_nms = "ag",
#'   weights = c(200, 300, 400, 100)
#' )
#'
#' @importFrom data.table setDT := .SD set alloc.col setcolorder setkeyv
direct_adjusted_estimates <- function(
  stats_dt,
  specs_dt,
  stratum_col_nms = NULL,
  adjust_col_nms,
  weights
) {

  # assertions -----------------------------------------------------------------
  assert_is_data_table(stats_dt)
  assert_is_data_table_with_required_names(
    specs_dt,
    required_names = c("est", "var", "conf_lvl", "conf_method")
  )
  assert_is_character_nonNA_vector(specs_dt[["est"]])
  assert_is_character_vector(specs_dt[["var"]])
  assert_is_double_nonNA_vector(specs_dt[["conf_lvl"]])
  stopfinot(
    specs_dt[["conf_lvl"]] > 0,
    specs_dt[["conf_lvl"]] < 1
  )
  assert_is_data_table_with_required_names(
    stats_dt,
    required_names = c(specs_dt[["est"]], specs_dt[["var"]])
  )

  # compute weighted averages --------------------------------------------------
  weights_dt <- weights_arg_to_weights_dt(weights = weights,
                                          adjust_col_nms = adjust_col_nms)

  keep_col_nms <- c(stratum_col_nms, adjust_col_nms,
                    specs_dt[["est"]], setdiff(specs_dt[["var"]], NA))
  stats_dt <- data.table::setDT(lapply(keep_col_nms, function(col_nm) {
    stats_dt[[col_nm]]
  }))

  stat_col_nms <- c(specs_dt[["est"]], setdiff(specs_dt[["var"]], NA))
  i.weight <- NULL # to appease our lord and saviour, R CMD CHECK
  stats_dt[
    i = weights_dt,
    on = eval(adjust_col_nms),
    j = (stat_col_nms) := lapply(.SD, function(col) {
      col * i.weight
    })
  ]
  stats_dt <- stats_dt[
    j = lapply(.SD, sum),
    .SDcols = stat_col_nms,
    keyby = eval(stratum_col_nms)
  ]

  # confidence intervals -------------------------------------------------------
  lapply(1:nrow(specs_dt), function(i) {

    est_col_nm <- specs_dt[["est"]][i]
    var_col_nm <- specs_dt[["var"]][i]
    conf_lvl <- specs_dt[["conf_lvl"]][i]
    conf_method <- specs_dt[["conf_method"]][i]
    if (is.na(conf_lvl) || is.na(conf_method) || conf_method == "none") {
      return(NULL)
    }
    ci_dt <- confidence_interval(
      est = stats_dt[[est_col_nm]],
      var = stats_dt[[var_col_nm]],
      conf_lvl = conf_lvl,
      conf_method = conf_method
    )

    ci_col_nms <- paste0(est_col_nm, c("_lo", "_hi"))
    data.table::set(stats_dt, j = ci_col_nms, value = ci_dt)
    data.table::alloc.col(stats_dt)
    NULL
  })

  # final touches --------------------------------------------------------------
  ordered_stat_col_nms <- unlist(lapply(1:nrow(specs_dt), function(i) {
    est_col_nm <- specs_dt[["est"]]
    var_col_nm <- specs_dt[["var"]]
    ci_col_nms <- paste0(est_col_nm, c("_lo", "_hi"))

    stat_col_nms <- intersect(c(est_col_nm, var_col_nm, ci_col_nms),
                              names(stats_dt))
    stat_col_nms
  }))
  data.table::setcolorder(stats_dt, c(stratum_col_nms, ordered_stat_col_nms))
  data.table::setkeyv(stats_dt, stratum_col_nms)

  stats_dt[]
}





confidence_interval <- function(
  est,
  var,
  conf_lvl,
  conf_method
) {
  stop("TODO")
}








