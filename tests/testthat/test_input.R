## Input testing

# setup :: load input data bundled with package
dir <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
stopifnot(nchar(dir) > 1)
results <- read_happy(file.path(dirname(dir), "happy_demo"))

test_that("read_happy reads hap.py results to an R data structure", {
  expect_true(!is.null(results))
  expect_equal(class(results)[1], "happy_result")
  expect_equal(length(results$pr_curve), 7)
})

test_that("happy results can be combined into a single object", {
  results2 <- results
  expect_warning(results_list <- c(results, results2))
  expect_true(inherits(results_list, "happy_result_list"))
  expect_equal(length(results_list), 2)
})

test_that("happy_result_list can be extended with a new happy_result", {
  results2 <- results
  expect_warning(results_list <- c(results, results2))

  results3 <- results
  attr(results3, "from") <- "dummy"
  results_list <- c(results_list, results3)
  expect_true(inherits(results_list, "happy_result_list"))
  expect_equal(length(results_list), 3)
})
