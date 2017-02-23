## Input testing

test_that("read_happy reads hap.py results to an R data structure", {
  dir <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
  stopifnot(nchar(dir) > 1)
  results <- read_happy(file.path(dirname(dir), "happy_demo"))

  expect_true(!is.null(results))
  expect_equal(class(results)[1], "happy_result")
})
