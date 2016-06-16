with_errors_as <- errors_are <- function(are, exp) {
    tryCatch(exp, error = function(e) are)
}
errors_are_na <- function(exp) { errors_are(NA, exp) }
errors_are_null <- function(exp) { errors_are(NULL, exp) }
errors_are_false <- function(exp) { errors_are(FALSE, exp) }
errors_are_warnings <- function(exp) { tryCatch(exp, error = function(e) warning(e)) }
