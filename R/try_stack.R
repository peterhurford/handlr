#' Try evaluating an expression multiple times before erroring.
#'
#' @param num_tries numeric. The number of attempts.
#' @param sleep numeric. How long to wait between attempts.
#' @param expr expression. The expression to evaluate with retries.
#' @return TRUE if the expression gets evaluated successfully on some attempt.
#' @export
with_retries <- function(expr, num_tries = 1, sleep= 0.001) {
  num_tries <- max(num_tries, 1L)
  current_try <- 0
  while (current_try < num_tries) {
    out <- evaluate::try_capture_stack(expr, env = parent.frame())
    if (!methods::is(out, "error")) break
    current_try <- current_try + 1
    message(paste0("Attempt ", current_try, " failed."))
    if (current_try < num_tries) {
      cat("Sleeping for ", sleep, "seconds\n")
      Sys.sleep(sleep)
    }
  }
  if (methods::is(out, "error")) {
    stop(out)
  } else {
    out
  }
}

#' Evaluate an expression and exit R with an exit code if expr fails.
#'
#' @param expr expression. The expression to evaluate.
#' @param exit_code numeric. The value of the exit_code to exit R.
#' @return TRUE if the expression gets evaluated successfully; exit R otherwise.
#' @export
try_with_exit_code <- function(expr, exit_code = 1) {
  out <- evaluate::try_capture_stack(expr, env = parent.frame())
  if (methods::is(out, "error")) {
     q(save = "no", status = exit_code)
  } 
  out 
}

#' Evaluate an expression multiple times and if fails, exit R with an exit_code.
#' This is a wrapper that combines functionality of try_with_exit_code and with_retries.
#'
#' @param expr expression. The expression to evaluate.
#' @param num_tries numeric. The number of attempts.
#' @param sleep_secs numeric. How long to wait between attempts.
#' @param exit_code numeric. The value of the exit_code to exit R.
#' @return TRUE if the expression gets evaluated successfully; exit R otherwise.
#' @export
try_stack <- function(expr, num_tries = 1, sleep_secs = 0.001, exit_code = 1) {
    try_with_exit_code(
        with_retries(expr, num_tries = num_tries, sleep = sleep_secs), 
        exit_code = exit_code)
}