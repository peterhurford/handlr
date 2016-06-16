retry <- function(expr, times = 3) {
  tries <- 0
  repeat {
    tries <- tries + 1
    if (tries >= times) { break }
    outcome <- try(eval(expr), silent = TRUE)
    if (methods::is(outcome, "try-error")) { next } else { break }
  }
  outcome
}
