
context("Combining results")

# setup :: load input data bundled with package
dir <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
stopifnot(nchar(dir) > 1)
suppressMessages(results <- read_happy(file.path(dirname(dir), "happy_demo")))


test_that("happy results can be combined into a single object", {
  results2 <- results
  expect_warning(
    results_list <- c(results, results2)
  )
  expect_true(inherits(results_list, "happy_result_list"))
  expect_equal(length(results_list), 2)
})

test_that("happy_result_list can be extended with a new happy_result", {
  results2 <- results
  expect_warning(results_list <- c(results, results2))
  results3 <- results

  expect_warning(results_list <- c(results_list, results3))
  expect_true(inherits(results_list, "happy_result_list"))
  expect_equal(length(results_list), 3)
})

test_that("combining happy_results warns of redundancy", {
  results2 <- results
  expect_warning(results_list <- c(results, results2))

  results3 <- results
  attr(results3, "from") <- "dummy/path/to/happy"
  expect_silent(rl <- c(results2, results3))
})

context("Extracting tables")

test_that("summary tables can be extracted from list of happy_result", {
  other_results <- results
  attr(other_results, "from") <- "dummy/path/to/happy"
  result_list <- c(results, other_results)
  summary_table <- extract(result_list, "summary")
  expect_is(summary_table, "data.frame")
})
