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
      Sys.sleep(sleep_secs)
    }
  }
  if (methods::is(out, "error")) {
    stop(out)
  } else {
    out
  }
}

