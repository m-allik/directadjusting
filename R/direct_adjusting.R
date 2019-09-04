




#' @md
#' @title Direct Adjusted Estimates
#' @description Compute direct adjusted estimates from a table of statistics.
#' @param stats_dt `[data.table]` (mandatory, no default)
#'
#' a `data.table` containing estimates and variance estimates of statistics
#' @param stat_col_nms `[character]` (mandatory, no default)
#'
#' names of columns in `stats_dt` containing estimates (statistics);
#' `NA` statistics values cause also `NA` confidence intervals
#' @param var_col_nms `[character]` (optional, default `NULL`)
#'
#' - if `NULL`, no confidence intervals can (will) be computed
#' - if `character` vector, names of columns in `stats_dt` containing variance
#'   estimates of the statistics specified in `stat_col_nms` with one-to-one
#'   correspondence; `NA` elements in `var_col_nms` cause no confidence
#'   intervals to computed for those statistics;
#'   `NA` variance estimates in `stats_dt` cause `NA` confidence intervals;
#'   negative values cause an error; `Inf` values cause `c(-Inf, Inf)`
#'   intervals with confidence interval method `"identity"`, etc.
#' @param conf_lvls `[numeric]` (mandatory, default `0.95`)
#'
#' confidence levels for confidence intervals; you may specify each statistic
#' (see `stat_col_nms`) its own level by supplying a vector of values;
#' values other than between `(0, 1)` cause an error
#' @param conf_methods `[character]` (mandatory, default `"identity"`)
#'
#' method to compute confidence intervals; either one string (to be used for
#' all statistics) or a vector of strings, one for each element of
#' `stat_col_nms`; use `"none"` for statistics for which you do not want
#' confidence intervals to be calculated (or `NA` in `var_col_nms`);
#' see \code{\link{confidence_intervals}} for supported methods
#' @param stratum_col_nms `[NULL, character]` (optional, default `NULL`)
#'
#' names of columns in `stats_dt` by which statistics are stratified (and they
#' should be stratified by these columns after direct adjusting)
#' @param adjust_col_nms `[character]` (mandatory, no default)
#'
#' names of columns in `stats_dt` by which statistics are currently stratified
#' and by which the statistics should be adjusted (e.g. `"agegroup"`)
#' @template weights_arg
#'
#' @section Weights:
#'
#' The weights are scaled internally to sum to one, but they need to be positive
#' numbers (or zero). The scalingis performed separately by each unique
#' combination of `stratum_col_nms` columns. This allows you to have e.g.
#' two hierarhical variables, one used for adjusting and one for stratifying
#' output (such as 18 age groups of 5 years for adjusting and 4 larger age
#' groups for stratifying output). See **Examples**.
#'
#' @section Tabulation:
#'
#' Currently every pair of columns in `union(stratum_col_nms, adjust_col_nms)`
#' must be either
#'
#' - hierarhical: every level of B exists under exactly one level of A (or
#'   converse); e.g. regions `c(1, 1, 2, 2)` and sub-regions `c(1, 2, 3, 4)`
#' - cross-joined: every level of B is repeated for every level of A; e.g.
#'   sexes `c(1, 1, 2, 2)` and regions `c(1, 2, 1, 2)`
#'
#' This ensures that adjusting will be performed properly, i.e. the weights
#' are merged and used as intended.
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
#' hrs_by_sex <- rep(1:2, each = 4)
#' hrs_by_ag <- rep(c(0.75, 0.90, 1.10, 1.25), times = 2)
#' counts <- rpois(8, baseline * hrs_by_sex * hrs_by_ag)
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
#' my_adj_stats <- direct_adjusted_estimates(
#'   stats_dt = my_stats,
#'   stat_col_nms = "e",
#'   var_col_nms = "v",
#'   conf_lvls = 0.95,
#'   conf_methods = "log",
#'   stratum_col_nms = "sex",
#'   adjust_col_nms = "ag",
#'   weights = c(200, 300, 400, 100)
#' )
#'
#' # adjusted by smaller age groups, stratified by larger age groups
#' my_stats[, "ag2" := c(1,1, 2,2, 1,1, 2,2)]
#' my_adj_stats <- direct_adjusted_estimates(
#'   stats_dt = my_stats,
#'   stat_col_nms = "e",
#'   var_col_nms = "v",
#'   conf_lvls = 0.95,
#'   conf_methods = "log",
#'   stratum_col_nms = c("sex", "ag2"),
#'   adjust_col_nms = "ag",
#'   weights = c(200, 300, 400, 100)
#' )
#'
#' # survival example; see help("survival.formula")
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   library("survival")
#'   library("data.table")
#'   fit <- survfit(Surv(time, status) ~ x, data = aml)
#'   surv_stats <- summary(fit, times = 0:40)
#'   surv_dt <- data.table::data.table(
#'     x = surv_stats[["strata"]],
#'     time = surv_stats[["time"]],
#'     surv = surv_stats[["surv"]],
#'     var = surv_stats[["std.err"]] ** 2
#'   )
#'   surv_dt_adj <- direct_adjusted_estimates(
#'     stats_dt = surv_dt,
#'     stat_col_nms = "surv",
#'     var_col_nms = "var",
#'     conf_lvls = 0.95,
#'     conf_methods = "log-log",
#'     stratum_col_nms = "time",
#'     adjust_col_nms = "x",
#'     weights = c(600, 400)
#'   )
#'   print(surv_dt_adj, nrows = 10)
#'   matplot(
#'     y = surv_dt_adj[, .(surv, surv_lo, surv_hi)], 
#'     x = surv_dt_adj[["time"]], type = "s", col = 1, lty = 1,
#'     xlab = "time", ylab = "survival",
#'     main = "Survival with 95 % CIs"
#'   )
#' }
#'
#' @importFrom data.table setDT := .SD set alloc.col setcolorder setkeyv
#' setnames uniqueN
#' @export
direct_adjusted_estimates <- function(
  stats_dt,
  stat_col_nms,
  var_col_nms,
  stratum_col_nms = NULL,
  adjust_col_nms,
  conf_lvls,
  conf_methods,
  weights
) {

  call <- match.call()

  # assertions -----------------------------------------------------------------
  assert_is_data_table(stats_dt)
  assert_is_character_nonNA_vector(stat_col_nms)
  assert_is_data_table_with_required_names(
    stats_dt,
    required_names = stat_col_nms
  )
  assert_is_one_of(
    var_col_nms,
    fun_nms = c("assert_is_character_vector", "assert_is_NULL")
  )
  if (!is.null(var_col_nms)) {
    assert_is_data_table_with_required_names(
      stats_dt,
      required_names = setdiff(var_col_nms, NA_character_)
    )
    lapply(setdiff(var_col_nms, NA_character_), function(var_col_nm) {
      eval(substitute(stopifnot(
        stats_dt[[VCN]] >= 0 | is.na(stats_dt[[VCN]])
      ), list(VCN = var_col_nm)))
    })
  } else {
    var_col_nms <- rep(NA_character_, length(stat_col_nms))
  }
  assert_is_double_nonNA_vector(conf_lvls)
  stopifnot(
    conf_lvls > 0, conf_lvls < 1
  )
  assert_is_character_nonNA_vector(conf_methods)
  eval(substitute(stopifnot(
    conf_methods %in% ALLOWED
  ), list(ALLOWED = allowed_conf_methods())))

  # check that stratification makes sense --------------------------------------
  
  keep_col_nms <- setdiff(
    c(stratum_col_nms, adjust_col_nms, stat_col_nms, var_col_nms), NA_character_
  )
  stats_dt <- data.table::setDT(lapply(keep_col_nms, function(col_nm) {
    stats_dt[[col_nm]]
  }))
  data.table::setnames(stats_dt, keep_col_nms)
  if (length(stratum_col_nms) == 0L) {
    stratum_col_nms <- ".____TMP_STRATUM_DUMMY"
    on.exit(stats_dt[, ".____TMP_STRATUM_DUMMY" := NULL])
    stats_dt[, ".____TMP_STRATUM_DUMMY" := NA]
  }
  test_col_nms <- setdiff(
    union(stratum_col_nms, adjust_col_nms),
    ".____TMP_STRATUM_DUMMY"
  )
  if (length(test_col_nms) > 1) {
    stratum_col_nm_pairs <- combn(test_col_nms, m = 2L)
    lapply(1:ncol(stratum_col_nm_pairs), function(pair_no) {
      pair <- stratum_col_nm_pairs[, pair_no]
      udt <- unique(stats_dt, by = pair)
      
      un1 <- data.table::uniqueN(udt[[pair[1]]])
      un2 <- data.table::uniqueN(udt[[pair[2]]])
      is_cj <- nrow(udt) == un1 * un2
      if (is_cj) {
        return(NULL)
      }
      
      is_hierachical <- nrow(udt) %in% c(un1, un2)
      if (!is_hierachical) {
        stop(simpleError(
          paste0(
            "stratum / adjust column pair ", deparse(pair),
            " in stats_dt are not ",
            "hierarchical nor cross-joined; see ",
            "?direct_adjusted_estimates section Tabulation"
          ),
          call = call
        ))
      }
    })
  }

  # compute weighted averages --------------------------------------------------
  weights_dt <- weights_arg_to_weights_dt(weights = weights,
                                          stats_dt = stats_dt,
                                          adjust_col_nms = adjust_col_nms)

  i.weight <- NULL # to appease our lord and saviour, R CMD CHECK
  stats_dt[
    i = weights_dt,
    on = eval(adjust_col_nms),
    j = ".__TMP_W" := i.weight
  ]
  stats_dt[
    j = ".__TMP_W" := .__TMP_W / sum(.__TMP_W),
    by = eval(stratum_col_nms)
  ]
  value_col_nms <- c(stat_col_nms, setdiff(var_col_nms, NA))
  stats_dt <- stats_dt[
    j = (value_col_nms) := lapply(.SD, function(col) {
      col * .__TMP_W
    }),
    .SDcols = value_col_nms
    ]
  stats_dt <- stats_dt[
    j = lapply(.SD, sum),
    .SDcols = value_col_nms,
    keyby = eval(stratum_col_nms)
  ]

  # confidence intervals -------------------------------------------------------
  lapply(1:length(stat_col_nms), function(i) {

    stat_col_nm <- stat_col_nms[i]
    var_col_nm <- var_col_nms[i]
    conf_lvl <- conf_lvls[i]
    conf_method <- conf_methods[i]
    if (is.na(var_col_nm) || conf_method == "none") {
      return(NULL)
    }
    ci_dt <- confidence_intervals(
      statistics = stats_dt[[stat_col_nm]],
      variances = stats_dt[[var_col_nm]],
      conf_lvl = conf_lvl,
      conf_method = conf_method
    )

    ci_col_nms <- paste0(stat_col_nm, c("_lo", "_hi"))
    data.table::set(
      stats_dt, 
      j = ci_col_nms, 
      value = lapply(c("ci_lo", "ci_hi"), function(col_nm) {
        ci_dt[[col_nm]]
      })
    )
    data.table::alloc.col(stats_dt)
    NULL
  })

  # final touches --------------------------------------------------------------
  ordered_stat_col_nms <- unlist(lapply(1:length(stat_col_nms), function(i) {
    stat_col_nm <- stat_col_nms
    var_col_nm <- var_col_nms
    ci_col_nms <- paste0(stat_col_nm, c("_lo", "_hi"))

    stat_col_nms <- intersect(c(stat_col_nm, var_col_nm, ci_col_nms),
                              names(stats_dt))
    stat_col_nms
  }))
  data.table::setcolorder(stats_dt, c(stratum_col_nms, ordered_stat_col_nms))
  data.table::setkeyv(stats_dt, stratum_col_nms)
  adjust_col_nms <- setdiff(adjust_col_nms, ".____TMP_STRATUM_DUMMY")
  if (length(adjust_col_nms) == 0) {
    adjust_col_nms <- NULL
  }
  data.table::setattr(
    stats_dt, "direct_adjusting_meta",
    mget(c("call", "stat_col_nms", "var_col_nms", "stratum_col_nms",
           "adjust_col_nms", "conf_lvls", "conf_methods"))
  )

  stats_dt[]
}





allowed_conf_methods <- function() {
  c("none", "identity", "log", "log-log")
}
confidence_interval_expression <- function(conf_method) {
  assert_is_character_nonNA_atom(conf_method)
  stopifnot(
    conf_method %in% allowed_conf_methods()
  )
  math <- switch(
    conf_method,
    identity = quote(STAT + Z * STD_ERR),
    log = quote(STAT * exp(Z * STD_ERR / STAT)),
    `log-log` = quote( STAT ** exp(-Z * (STD_ERR / (abs(log(STAT)) * STAT)))),
    stop("No math defined for conf_method = ", deparse(conf_method))
  )
}
#' @md
#' @title Confidence Intervals
#' @description
#' Computes different kinds of confidence intervals given the statistics
#' and their variance estimates.
#' @param statistics `[numeric]` (mandatory, no default)
#' 
#' statistics for which to calculate confidence intervals
#' @param variances `[numeric]` (mandatory, no default)
#' 
#' variance estimates of `statistics` used to compute confidence intervals
#' @param conf_lvl `[numeric]` (mandatory, default `0.95`)
#' 
#' confidence level of confidence intervals in `]0, 1[`
#' @param conf_method `[character]` (mandatory, default `"identity"`)
#' 
#' see section **Confidence interval methods**
#' @importFrom data.table := setattr setnames set
#' @eval {
#' conf_methods <- setdiff(allowed_conf_methods(), "none")
#' maths <- vapply(conf_methods, function(conf_method) {
#'   paste0(deparse(confidence_interval_expression(conf_method)), collapse = "")
#' }, character(1))
#' c(
#'   "@section Confidence interval methods:\n",
#'   "Currently supported confidence interval methods and their formulae:\n",
#'   "\\itemize{",
#'   paste0(" \\item ", conf_methods, ": ", maths, collapse = ""),
#'   "}",
#'   "\n",
#'   "Where\n",
#'   "\\itemize{",
#'   " \\item STAT is the statistic,\n",
#'   " \\item Z is the quantile from the standard normal distribution for the ",
#'   "   lower or upper bound, and\n",
#'   " \\item STD_ERR is the standard error (square root of the variance)\n",
#'   "}"
#' )
#' }
#' @return 
#' `data.table` with columns
#' - `statistic`: the values you supplied via argument `statistics`
#' - `variance`: the values you supplied via argument `variances`
#' - `ci_lo`: lower bound of confidence interval
#' - `ci_hi`: upper bound of confidence interval
#' @export
confidence_intervals <- function(
  statistics,
  variances,
  conf_lvl = 0.95,
  conf_method = "identity"
) {
  assert_is_number_vector(statistics)
  assert_is_number_vector(variances)
  assert_is_double_nonNA_atom(conf_lvl)
  assert_is_character_nonNA_atom(conf_method)
  eval(substitute(stopifnot(
    variances >= 0 | is.na(variances),
    conf_lvl > 0,
    conf_lvl < 1,
    conf_method %in% ALLOWED
  ), list(ALLOWED = setdiff(allowed_conf_methods(), "none"))))

  math <- confidence_interval_expression(conf_method = conf_method)

  dt <- data.table::setDT(list(STAT = statistics, STD_ERR = sqrt(variances)))
  Z <- qnorm(p = (1 - conf_lvl) / 2)
  expr <- substitute(dt[, "ci_lo" := MATH], list(MATH = math))
  eval(expr)
  Z <- qnorm(p = conf_lvl + (1 - conf_lvl) / 2)
  expr <- substitute(dt[, "ci_hi" := MATH], list(MATH = math))
  eval(expr)

  data.table::setattr(
    dt, name = "ci_meta", value = mget(c("conf_lvl", "conf_method", "math"))
  )
  data.table::setnames(dt, c("STAT", "STD_ERR"), c("statistic", "variance"))
  data.table::set(
    dt,
    j = "variance",
    value = dt[["variance"]] ** 2
  )
  data.table::setcolorder(dt, c("statistic", "variance", "ci_lo", "ci_hi"))
  dt[]
}








