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
      with_retries({ x <- pass_on_third_attempt() }, num_tries = 3, sleep = 0),
      "didn't error"
    )
    expect_equal(x, "didn't error")
  })

  test_that("it only retries the prescribed number of times", {
    attempt_number <<- 1
    expect_error(
      with_retries({ x <- pass_on_third_attempt() }, num_tries = 2, sleep = 0),
      "not enough retries"
    )
  })
})

describe("try_with_exit_code", {
    test_that("it evaluates expr successfully", {
    expect_equal(try_with_exit_code(sum(1, 2)), 3)
  })
    test_that("Expr evaluation fails and it exits R with specific exit code", {
    with_mock(
      `q` = function(status_code, ...) { stop(status_code) },
      expect_error(try_with_exit_code(sum("someval"), exit_code = 300), "300")
    )
  })
})