describe("with_retries", {

  attempt_number <- "assigned below"
  pass_on_third_attempt <- function() {
    if (attempt_number < 3) {
      attempt_number <<- attempt_number + 1
      stop("not enough retries")  
    }
    "didn't error"
  }
    
  test_that("it tries to evaluate an expression multiple times", {
    attempt_number <<- 1
    expect_equal(
      with_retries({ x <- pass_on_third_attempt() }, num_tries = 3, sleep_secs = 0),
      "didn't error"
    )
    expect_equal(x, "didn't error")
  })

  test_that("it only retries the prescribed number of times", {
    attempt_number <<- 1
    expect_error(
      with_retries({ x <- pass_on_third_attempt() }, num_tries = 2, sleep_secs = 0),
      "not enough retries"
    )  
  })


})
