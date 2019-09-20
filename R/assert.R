




# one assertion can test for
# - class (data type)
# - atom / vector / matrix / other (length + class)
# - numeric value range (>= 0, >0, etc.)



raise_internal_error_if_not <- function(...) {
  test_exprs <- substitute(list(...))
  test_results <- list(...)
  lapply(seq_along(test_results), function(i) {
    test_result <- test_results[[i]]
    if (!is.logical(test_result)) {
      stop("test ", deparse(test_exprs[[i]]),
           " did not evaluate to logical values; ",
           "result had class(es) ", deparse(class(test_result)))
    } else if (!all(test_result)) {
      stop("not all were TRUE: ", deparse(test_exprs[[i]]))
    }
  })
  invisible(NULL)
}



assert_is_one_of <- function(x, x_nm = NULL, fun_nms, funs_arg_list = NULL) {
  raise_internal_error_if_not(
    length(fun_nms) > 0,
    is.character(fun_nms),
    !is.na(fun_nms),
    vapply(fun_nms, function(fun_nm) {
      is.function(get(fun_nm))
    }, logical(1)),

    is.null(funs_arg_list) || inherits(funs_arg_list, "list")
  )

  funs <- lapply(fun_nms, match.fun)
  funs_arg_list <- as.list(funs_arg_list)
  tries <- lapply(funs, function(fun) {
    arg_list <- formals(fun)
    arg_list[c("x", "x_nm")] <- list(x = quote(x), x_nm = quote(x_nm))
    supplied_arg_nms <- intersect(names(arg_list), names(funs_arg_list))
    arg_list[supplied_arg_nms] <- funs_arg_list[supplied_arg_nms]
    tryCatch(
      do.call(fun, arg_list),
      error = function(e) {
        e
      }
    )
  })

  is_error <- vapply(tries, inherits, logical(1), what = "error")
  if (all(is_error)) {
    error_msgs <- vapply(tries, function(error_obj) {
      paste0(error_obj[["message"]], collapse = "")
    }, character(1))
    error_msgs <- paste0("    - ", fun_nms, ": ", error_msgs, collapse = "\n")
    stop("at least one of the following assertions must pass:\n", error_msgs)
  }

  invisible(NULL)
}

assert_has_class <- function(x, x_nm = NULL, required_class) {
  raise_internal_error_if_not(
    length(required_class) == 1,
    is.character(required_class),
    !is.na(required_class)
  )
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }

  if (!inherits(x, required_class)) {
    stop("expected ", deparse(x_nm), " to have class ", deparse(required_class),
         " but it had class(es) ", deparse(class(x)))
  }

  invisible(NULL)
}

assert_has_one_of_classes <- function(x, x_nm = NULL, classes) {
  stopifnot(
    length(classes) > 0,
    is.character(classes),
    !is.na(classes)
  )
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }

  if (!inherits(x, classes)) {
    stop("expected ", deparse(x_nm), " to have one of classes ",
         deparse(classes),
         " but it had class(es) ", deparse(class(x)))
  }

  invisible(NULL)
}

assert_is_number <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  if (!is.numeric(x)) {
    stop(deparse(x_nm), " is not a number")
  }
  invisible(NULL)
}
assert_is_character <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  if (!is.character(x)) {
    stop(deparse(x_nm), " is not a character string object ",
         "(see ?\"character\")")
  }
  invisible(NULL)
}
assert_is_double <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  if (!is.double(x)) {
    stop(deparse(x_nm), " is not a double object (see ?\"double\")")
  }
  invisible(NULL)
}
assert_is_integer <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  if (!is.integer(x)) {
    stop(deparse(x_nm), " is not integer (see ?\"integer\")")
  }
  invisible(NULL)
}
assert_is_logical <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  if (!is.logical(x)) {
    stop(deparse(x_nm), " is not logical (see ?\"logical\")")
  }
  invisible(NULL)
}
assert_is_Date <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  if (!inherits(x, "Date")) {
    stop(deparse(x_nm), " is not a Date object (see ?\"Date\")")
  }
  invisible(NULL)
}

assert_is_atom <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  if (length(x) != 1) {
    stop(deparse(x_nm), " had length ", length(x), " but expected it to have ",
         "length 1")
  }
  invisible(NULL)
}

assert_is_vector <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  if (!is.null(dim(x))) {
    stop(deparse(x_nm), " did not have NULL dim")
  }
  if (is.list(x)) {
    stop(deparse(x_nm), " is list-like, not a vector")
  }
  if (!is.vector(x)) {
    stop(deparse(x_nm), " does not pass is.vector(x) test")
  }
  invisible(NULL)
}

assert_is_matrix <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_has_class(x = x, x_nm = x_nm, required_class = "matrix")
  invisible(NULL)
}

assert_is_data.frame <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_has_class(x = x, x_nm = x_nm, required_class = "data.frame")
  invisible(NULL)
}

assert_is_data.table <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_has_class(x = x, x_nm = x_nm, required_class = "data.table")
  invisible(NULL)
}

assert_is_nonNA <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  n_na <- sum(is.na(x))
  if (n_na > 0) {
    stop(deparse(x_nm), " should not have NA values; it had ", n_na,
         " NA values")
  }
  invisible(NULL)
}

assert_is_NULL <- function(
  x,
  x_nm = NULL
) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  if (!is.null(x)) {
    stop(deparse(x_nm), " should be NULL; it had class(es) ",
         deparse(class(x)))
  }
}

assert_is_list <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_has_class(x = x, x_nm = x_nm, required_class = "list")
}

assert_is_data_table <- function(
  x,
  x_nm = NULL
) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_has_class(x = x, x_nm = x_nm, required_class = "data.table")
}

assert_is_data_table_with_required_names <- function(
  x,
  x_nm = NULL,
  required_names
) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_data.table(x)
  assert_is_character_nonNA_vector(required_names)
  miss_nms <- setdiff(required_names, names(x))
  if (length(miss_nms)) {
    stop(deparse(x_nm), " did not have these required names: ",
         deparse(miss_nms))
  }
  invisible(NULL)
}

assert_is_between_inclusive <- function(x, x_nm = NULL, lo, hi) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_number(x)
  n_not_between <- sum(!data.table::between(
    x = x, lower = lo, upper = hi, incbounds = TRUE
  ))
  if (n_not_between > 0) {
    stop(n_not_between, " values in ", deparse(x_nm), " were not between ",
         lo, " and ", hi, " (inclusive bounds [a, b])")
  }
}
assert_is_between_exclusive <- function(x, x_nm = NULL, lo, hi) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_number(x)
  n_not_between <- sum(!data.table::between(
    x = x, lower = lo, upper = hi, incbounds = FALSE
  ))
  if (n_not_between > 0) {
    stop(n_not_between, " values in ", deparse(x_nm), " were not between ",
         lo, " and ", hi, " (exclusive bounds ]a, b[)")
  }
}
assert_is_gte <- function(x, x_nm = NULL, lo) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_number(x)
  n_not_pass <- sum(x < lo)
  if (n_not_pass > 0) {
    stop(n_not_pass, " values in ", deparse(x_nm), " were not >= ", lo)
  }
}
assert_is_gt <- function(x, x_nm = NULL, lo) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_number(x)
  n_not_pass <- sum(x <= lo)
  if (n_not_pass > 0) {
    stop(n_not_pass, " values in ", deparse(x_nm), " were not > ", lo)
  }
}
assert_is_lte <- function(x, x_nm = NULL, hi) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_number(x)
  n_not_pass <- sum(x > hi)
  if (n_not_pass > 0) {
    stop(n_not_pass, " values in ", deparse(x_nm), " were not <= ", hi)
  }
}
assert_is_lt <- function(x, x_nm = NULL, hi) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_number(x)
  n_not_pass <- sum(x >= hi)
  if (n_not_pass > 0) {
    stop(n_not_pass, " values in ", deparse(x_nm), " were not < ", hi)
  }
}
assert_is_ltezero <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_lte(x = x, x_nm = x_nm, hi = 0.0)
}
assert_is_ltzero <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_lt(x = x, x_nm = x_nm, hi = 0.0)
}
assert_is_gtezero <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_gte(x = x, x_nm = x_nm, lo = 0.0)
}
assert_is_gtzero <- function(x, x_nm = NULL) {
  if (is.null(x_nm)) {
    x_nm <- deparse(substitute(x))
  }
  assert_is_gte(x = x, x_nm = x_nm, lo = 0.0)
}

#' @importFrom data.table := CJ setkeyv
generate_assertions <- function() {
  # developer, you will call this function to auto-generate assertion
  # functions at the end of this script.

  lines <- readLines("R/assert.R")

  start_line <- paste0("# ---- only auto-generated assertion functions ",
                       "below this line -----------------")
  wh_start <- which(lines == start_line)
  lines <- c(lines[1:wh_start], rep("", 5))

  levels <- list(
    c("double", "number", "integer", "Date", "character", "logical"),
    "_",
    c("nonNA", ""),
    "_",
    c("gtezero", "gtzero", "ltezero", "ltzero", ""),
    "_",
    c("atom", "vector", "matrix")
  )

  fun_nm_dt <- do.call(data.table::CJ, levels)
  data.table::setkeyv(fun_nm_dt, names(fun_nm_dt))
  fun_nm_dt <- fun_nm_dt[
    !(fun_nm_dt[["V1"]] %in% c("character", "logical", "Date") &
        fun_nm_dt[["V5"]] != ""),
    ]
  fun_nms <- do.call(paste0, fun_nm_dt)
  fun_nms <- paste0("assert_is_", fun_nms)
  fun_nms <- gsub("_{1,}", "_", fun_nms)

  fun_nm_dt[, c("V2", "V4", "V6") := NULL]
  fun_nm_dt[, names(fun_nm_dt) := lapply(.SD, function(col) {
    fun_nms <- paste0("assert_is_", col)
    fun_calls <- paste0(fun_nms, "(x = x, x_nm = x_nm)")
    fun_calls[col == ""] <- ""
    fun_calls
  })]

  fun_definitions <- unlist(lapply(seq_along(fun_nms), function(i) {
    fun_nm <- fun_nms[i]
    def <- paste0(fun_nm, " <- function(x, x_nm = NULL) {")
    def <- c(def, paste0("  ", setdiff(as.character(fun_nm_dt[i, ]), "")))
    def <- c(def, "}", rep("", 5))
  }))

  lines <- c(lines, fun_definitions)

  writeLines(text = lines, con = "R/assert.R")
  invisible(NULL)
}
generate_assertions()


# ---- only auto-generated assertion functions below this line -----------------





assert_is_Date_atom <- function(x, x_nm = NULL) {
  assert_is_Date(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_Date_matrix <- function(x, x_nm = NULL) {
  assert_is_Date(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_Date_vector <- function(x, x_nm = NULL) {
  assert_is_Date(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_Date_nonNA_atom <- function(x, x_nm = NULL) {
  assert_is_Date(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_Date_nonNA_matrix <- function(x, x_nm = NULL) {
  assert_is_Date(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_Date_nonNA_vector <- function(x, x_nm = NULL) {
  assert_is_Date(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_character_atom <- function(x, x_nm = NULL) {
  assert_is_character(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_character_matrix <- function(x, x_nm = NULL) {
  assert_is_character(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_character_vector <- function(x, x_nm = NULL) {
  assert_is_character(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_character_nonNA_atom <- function(x, x_nm = NULL) {
  assert_is_character(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_character_nonNA_matrix <- function(x, x_nm = NULL) {
  assert_is_character(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_character_nonNA_vector <- function(x, x_nm = NULL) {
  assert_is_character(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_double_atom <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_double_matrix <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_double_vector <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_double_gtezero_atom <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_double_gtezero_matrix <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_double_gtezero_vector <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_double_gtzero_atom <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_double_gtzero_matrix <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_double_gtzero_vector <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_double_ltezero_atom <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_double_ltezero_matrix <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_double_ltezero_vector <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_double_ltzero_atom <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_double_ltzero_matrix <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_double_ltzero_vector <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_atom <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_matrix <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_vector <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_gtezero_atom <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_gtezero_matrix <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_gtezero_vector <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_gtzero_atom <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_gtzero_matrix <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_gtzero_vector <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_ltezero_atom <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_ltezero_matrix <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_ltezero_vector <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_ltzero_atom <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_ltzero_matrix <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_double_nonNA_ltzero_vector <- function(x, x_nm = NULL) {
  assert_is_double(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_integer_atom <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_integer_matrix <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_integer_vector <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_integer_gtezero_atom <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_integer_gtezero_matrix <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_integer_gtezero_vector <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_integer_gtzero_atom <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_integer_gtzero_matrix <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_integer_gtzero_vector <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_integer_ltezero_atom <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_integer_ltezero_matrix <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_integer_ltezero_vector <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_integer_ltzero_atom <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_integer_ltzero_matrix <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_integer_ltzero_vector <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_atom <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_matrix <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_vector <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_gtezero_atom <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_gtezero_matrix <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_gtezero_vector <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_gtzero_atom <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_gtzero_matrix <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_gtzero_vector <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_ltezero_atom <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_ltezero_matrix <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_ltezero_vector <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_ltzero_atom <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_ltzero_matrix <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_integer_nonNA_ltzero_vector <- function(x, x_nm = NULL) {
  assert_is_integer(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_logical_atom <- function(x, x_nm = NULL) {
  assert_is_logical(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_logical_matrix <- function(x, x_nm = NULL) {
  assert_is_logical(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_logical_vector <- function(x, x_nm = NULL) {
  assert_is_logical(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_logical_nonNA_atom <- function(x, x_nm = NULL) {
  assert_is_logical(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_logical_nonNA_matrix <- function(x, x_nm = NULL) {
  assert_is_logical(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_logical_nonNA_vector <- function(x, x_nm = NULL) {
  assert_is_logical(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_number_atom <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_number_matrix <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_number_vector <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_number_gtezero_atom <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_number_gtezero_matrix <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_number_gtezero_vector <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_number_gtzero_atom <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_number_gtzero_matrix <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_number_gtzero_vector <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_number_ltezero_atom <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_number_ltezero_matrix <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_number_ltezero_vector <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_number_ltzero_atom <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_number_ltzero_matrix <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_number_ltzero_vector <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_atom <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_matrix <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_vector <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_gtezero_atom <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_gtezero_matrix <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_gtezero_vector <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_gtzero_atom <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_gtzero_matrix <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_gtzero_vector <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_gtzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_ltezero_atom <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_ltezero_matrix <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_ltezero_vector <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltezero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_ltzero_atom <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_ltzero_matrix <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_matrix(x = x, x_nm = x_nm)
}





assert_is_number_nonNA_ltzero_vector <- function(x, x_nm = NULL) {
  assert_is_number(x = x, x_nm = x_nm)
  assert_is_nonNA(x = x, x_nm = x_nm)
  assert_is_ltzero(x = x, x_nm = x_nm)
  assert_is_vector(x = x, x_nm = x_nm)
}





