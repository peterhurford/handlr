#' Convert errors into another R object.
#'
#' Instead of erroring, that R object will be returned instead.
#'
#' @param are object. The R object to return.
#' @param exp expression. The expression to run.
#' @export
with_errors_as <- errors_are <- function(are, exp) {
    tryCatch(exp, error = function(e) are)
}

#' Convert errors to NA.
#'
#' @param exp expresion. The expression to run.
#' @export
errors_are_na <- function(exp) { errors_are(NA, exp) }

#' Convert errors to NULL.
#'
#' @param exp expresion. The expression to run.
#' @export
errors_are_null <- function(exp) { errors_are(NULL, exp) }

#' Convert errors to FALSE.
#'
#' @param exp expresion. The expression to run.
#' @export
errors_are_false <- function(exp) { errors_are(FALSE, exp) }

#' Convert errors to warnings.
#'
#' @param exp expresion. The expression to run.
#' @export
errors_are_warnings <- function(exp) { tryCatch(exp, error = function(e) warning(e)) }
