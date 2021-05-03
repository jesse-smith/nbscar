#' Verify Whether RStudio is Available Via API
#'
#' @param version_needed Optional minimal RStudio version
#'
#' @return `TRUE` if the rstudioapi package is installed and RStudio is running;
#'   otherwise, `FALSE`
#'
#' @keywords internal
verify_rstudio <- function(version_needed = '0.99.853') {
  if (rlang::is_empty(version_needed)) {
    version_needed <- NULL
  } else {
    assert_string(version_needed)
  }

  if (rlang::is_true(rlang::is_installed('rstudioapi'))) {
    rlang::is_true(rstudioapi::isAvailable(version_needed))
  } else {
    FALSE
  }
}


#' Standardize a Class
#'
#' Combine class names and remove missing values
#'
#' @param ... `character` vectors of class names
std_class <- function(...) {

  chr_reduce <- rlang::as_function(~ vec_c(.x, .y, .ptype = character()))

  not_na <- rlang::as_function(~ rlang::is_true(!rlang::is_na(.x)))

  class <- Filter(not_na, Reduce(chr_reduce, rlang::list2(...)))

  if (vec_is_empty(class)) character() else vec_set_names(vec_data(class), NULL)
}

#' Assertions
#'
#' @description
#' `assert_string()` asserts that an object is a length 1 `character` vector
#'
#' `assert_bool()` asserts that an object is a length 1 `logical` that is either
#' `TRUE` or `FALSE`
#'
#' `assert_scalar_int()` asserts than an object is a scalar `integer`
#'   (or integerish)
#'
#' @param x The object to check
#'
#' @param allow_empty Boolean. Should `assert_string()` allow an empty string?
#'   The default is `TRUE`.
#'
#' @param integerish Boolean. Should `assert_scalar_int()` allow `x` to be
#'   integerish, rather than a strict `integer`?
#'
#' @return The input (or an error if assertion is failed)
#'
#' @keywords internal
#'
#' @name assert
NULL

#' @rdname assert
#'
#' @keywords internal
assert_string <- function(x, allow_empty = TRUE) {
  assert_bool(allow_empty)

  arg <- paste0('`', as.character(substitute(x)), '`')

  if (!rlang::is_string(x)) {
    rlang::abort(paste(arg, 'must be a string'))
  } else if (!nzchar(x) && allow_empty) {
    rlang::abort(paste(arg, 'must be a non-empty string'))
  }

  x
}

#' @rdname assert
#'
#' @keywords internal
assert_bool <- function(x) {
  if (!rlang::is_bool(x)) {
    arg <- paste0('`', rlang::expr_label(rlang::enexpr(x)), '`')
    rlang::abort(
      paste(arg, 'must be boolean (either `TRUE` or `FALSE` and length 1)')
    )
  }

  x
}

#' @rdname assert
#'
#' @keywords internal
assert_scalar_int <- function(x, integerish = FALSE) {
  assert_bool(integerish)

  if (integerish) {
    is_scalar_int <- rlang::is_scalar_integerish(x)
  } else {
    is_scalar_int <- rlang::is_scalar_integer(x)
  }

  if (!is_scalar_int) {
    arg <- rlang::expr_label(rlang::enexpr(x))
    rlang::abort(
      paste(arg, 'must be a scalar integer')
    )
  }
}
