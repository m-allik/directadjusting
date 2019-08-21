




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





assert_has_class <- function(x, x.nm = NULL, required.class) {
  raise_internal_error_if_not(
    length(required.class) == 1,
    is.character(required.class),
    !is.na(required.class)
  )
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }

  if (!inherits(x, required.class)) {
    stop("expected ", deparse(x.nm), " to have class ", deparse(required.class),
         " but it had class(es) ", deparse(class(x)))
  }

  invisible(NULL)
}

assert_has_one_of_classes <- function(x, x.nm = NULL, classes) {
  stopifnot(
    length(classes) > 0,
    is.character(classes),
    !is.na(classes)
  )
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }

  if (!inherits(x, classes)) {
    stop("expected ", deparse(x.nm), " to have one of classes ",
         deparse(classes),
         " but it had class(es) ", deparse(class(x)))
  }

  invisible(NULL)
}

assert_is_number <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  if (!is.numeric(x)) {
    stop(deparse(x.nm), " is not a number")
  }
  invisible(NULL)
}
assert_is_character <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  if (!is.character(x)) {
    stop(deparse(x.nm), " is not a character string object ",
         "(see ?\"character\")")
  }
  invisible(NULL)
}
assert_is_double <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  if (!is.double(x)) {
    stop(deparse(x.nm), " is not a double object (see ?\"double\")")
  }
  invisible(NULL)
}
assert_is_integer <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  if (!is.integer(x)) {
    stop(deparse(x.nm), " is not integer (see ?\"integer\")")
  }
  invisible(NULL)
}
assert_is_logical <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  if (!is.logical(x)) {
    stop(deparse(x.nm), " is not logical (see ?\"logical\")")
  }
  invisible(NULL)
}


assert_is_atom <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  if (length(x) != 1) {
    stop(deparse(x.nm), " had length ", length(x), " but expected it to have ",
         "length 1")
  }
  invisible(NULL)
}

assert_is_vector <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  if (!is.null(dim(x))) {
    stop(deparse(x.nm), " did not have NULL dim")
  }
  if (is.list(x)) {
    stop(deparse(x.nm), " is list-like, not a vector")
  }
  if (!is.vector(x)) {
    stop(deparse(x.nm), " does not pass is.vector(x) test")
  }
  invisible(NULL)
}

assert_is_matrix <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  assert_has_class(x = x, x.nm = x.nm, required.class = "matrix")
  invisible(NULL)
}

assert_is_data.frame <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  assert_has_class(x = x, x.nm = x.nm, required.class = "data.frame")
  invisible(NULL)
}

assert_is_data.table <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  assert_has_class(x = x, x.nm = x.nm, required.class = "data.table")
  invisible(NULL)
}

assert_is_nonNA <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  n_na <- sum(is.na(x))
  if (n_na > 0) {
    stop(deparse(x.nm), " should not have NA values; it had ", n_na,
         " NA values")
  }
  invisible(NULL)
}

assert_is_data_table_with_required_names <- function(
  x,
  x.nm = NULL,
  required.names
) {
  assert_is_data.table(x)
  assert_is_character_nonNA_vector(required.names)
  miss_nms <- setdiff(names(x), required.names)
  if (length(miss_nms)) {
    stop(deparse(x.nm), " did not have these required names: ",
         deparse(miss_nms))
  }
  invisible(NULL)
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
    c("double", "number", "integer", "character", "logical"),
    "_",
    c("nonNA", ""),
    "_",
    c("atom", "vector", "matrix")
  )

  fun_nm_dt <- do.call(data.table::CJ, levels)
  data.table::setkeyv(fun_nm_dt, names(fun_nm_dt))
  fun_nms <- do.call(paste0, fun_nm_dt)
  fun_nms <- paste0("assert_is_", fun_nms)
  fun_nms <- gsub("_{1,}", "_", fun_nms)

  fun_nm_dt[, c("V2", "V4") := NULL]
  fun_nm_dt[, names(fun_nm_dt) := lapply(.SD, function(col) {
    fun_nms <- paste0("assert_is_", col)
    fun_calls <- paste0(fun_nms, "(x = x, x.nm = x.nm)")
    fun_calls[col == ""] <- ""
    fun_calls
  })]

  fun_definitions <- unlist(lapply(seq_along(fun_nms), function(i) {
    fun_nm <- fun_nms[i]
    def <- paste0(fun_nm, " <- function(x, x.nm = NULL) {")
    def <- c(def, paste0("  ", setdiff(as.character(fun_nm_dt[i, ]), "")))
    def <- c(def, "}", rep("", 5))
  }))

  lines <- c(lines, fun_definitions)

  writeLines(text = lines, con = "R/assert.R")
  invisible(NULL)
}



# ---- only auto-generated assertion functions below this line -----------------





assert_is_character_atom <- function(x, x.nm = NULL) {
  assert_is_character(x = x, x.nm = x.nm)
  assert_is_atom(x = x, x.nm = x.nm)
}





assert_is_character_matrix <- function(x, x.nm = NULL) {
  assert_is_character(x = x, x.nm = x.nm)
  assert_is_matrix(x = x, x.nm = x.nm)
}





assert_is_character_vector <- function(x, x.nm = NULL) {
  assert_is_character(x = x, x.nm = x.nm)
  assert_is_vector(x = x, x.nm = x.nm)
}





assert_is_character_nonNA_atom <- function(x, x.nm = NULL) {
  assert_is_character(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_atom(x = x, x.nm = x.nm)
}





assert_is_character_nonNA_matrix <- function(x, x.nm = NULL) {
  assert_is_character(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_matrix(x = x, x.nm = x.nm)
}





assert_is_character_nonNA_vector <- function(x, x.nm = NULL) {
  assert_is_character(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_vector(x = x, x.nm = x.nm)
}





assert_is_double_atom <- function(x, x.nm = NULL) {
  assert_is_double(x = x, x.nm = x.nm)
  assert_is_atom(x = x, x.nm = x.nm)
}





assert_is_double_matrix <- function(x, x.nm = NULL) {
  assert_is_double(x = x, x.nm = x.nm)
  assert_is_matrix(x = x, x.nm = x.nm)
}





assert_is_double_vector <- function(x, x.nm = NULL) {
  assert_is_double(x = x, x.nm = x.nm)
  assert_is_vector(x = x, x.nm = x.nm)
}





assert_is_double_nonNA_atom <- function(x, x.nm = NULL) {
  assert_is_double(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_atom(x = x, x.nm = x.nm)
}





assert_is_double_nonNA_matrix <- function(x, x.nm = NULL) {
  assert_is_double(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_matrix(x = x, x.nm = x.nm)
}





assert_is_double_nonNA_vector <- function(x, x.nm = NULL) {
  assert_is_double(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_vector(x = x, x.nm = x.nm)
}





assert_is_integer_atom <- function(x, x.nm = NULL) {
  assert_is_integer(x = x, x.nm = x.nm)
  assert_is_atom(x = x, x.nm = x.nm)
}





assert_is_integer_matrix <- function(x, x.nm = NULL) {
  assert_is_integer(x = x, x.nm = x.nm)
  assert_is_matrix(x = x, x.nm = x.nm)
}





assert_is_integer_vector <- function(x, x.nm = NULL) {
  assert_is_integer(x = x, x.nm = x.nm)
  assert_is_vector(x = x, x.nm = x.nm)
}





assert_is_integer_nonNA_atom <- function(x, x.nm = NULL) {
  assert_is_integer(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_atom(x = x, x.nm = x.nm)
}





assert_is_integer_nonNA_matrix <- function(x, x.nm = NULL) {
  assert_is_integer(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_matrix(x = x, x.nm = x.nm)
}





assert_is_integer_nonNA_vector <- function(x, x.nm = NULL) {
  assert_is_integer(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_vector(x = x, x.nm = x.nm)
}





assert_is_logical_atom <- function(x, x.nm = NULL) {
  assert_is_logical(x = x, x.nm = x.nm)
  assert_is_atom(x = x, x.nm = x.nm)
}





assert_is_logical_matrix <- function(x, x.nm = NULL) {
  assert_is_logical(x = x, x.nm = x.nm)
  assert_is_matrix(x = x, x.nm = x.nm)
}





assert_is_logical_vector <- function(x, x.nm = NULL) {
  assert_is_logical(x = x, x.nm = x.nm)
  assert_is_vector(x = x, x.nm = x.nm)
}





assert_is_logical_nonNA_atom <- function(x, x.nm = NULL) {
  assert_is_logical(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_atom(x = x, x.nm = x.nm)
}





assert_is_logical_nonNA_matrix <- function(x, x.nm = NULL) {
  assert_is_logical(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_matrix(x = x, x.nm = x.nm)
}





assert_is_logical_nonNA_vector <- function(x, x.nm = NULL) {
  assert_is_logical(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_vector(x = x, x.nm = x.nm)
}





assert_is_number_atom <- function(x, x.nm = NULL) {
  assert_is_number(x = x, x.nm = x.nm)
  assert_is_atom(x = x, x.nm = x.nm)
}





assert_is_number_matrix <- function(x, x.nm = NULL) {
  assert_is_number(x = x, x.nm = x.nm)
  assert_is_matrix(x = x, x.nm = x.nm)
}





assert_is_number_vector <- function(x, x.nm = NULL) {
  assert_is_number(x = x, x.nm = x.nm)
  assert_is_vector(x = x, x.nm = x.nm)
}





assert_is_number_nonNA_atom <- function(x, x.nm = NULL) {
  assert_is_number(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_atom(x = x, x.nm = x.nm)
}





assert_is_number_nonNA_matrix <- function(x, x.nm = NULL) {
  assert_is_number(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_matrix(x = x, x.nm = x.nm)
}





assert_is_number_nonNA_vector <- function(x, x.nm = NULL) {
  assert_is_number(x = x, x.nm = x.nm)
  assert_is_nonNA(x = x, x.nm = x.nm)
  assert_is_vector(x = x, x.nm = x.nm)
}





