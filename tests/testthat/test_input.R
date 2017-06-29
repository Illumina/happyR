## Input testing

context("Load hap.py results")

# setup :: load input data bundled with package
dir <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
stopifnot(nchar(dir) > 1)
suppressMessages(results <- read_happy(file.path(dirname(dir), "happy_demo")))

test_that("read_happy reads hap.py results to an R data structure", {
  expect_true(!is.null(results))
  expect_equal(class(results)[1], "happy_result")
  expect_equal(length(results$pr_curve), 7)
})

test_that("PR curve data is of class happy_roc", {
  for (name in names(results$pr_curve)) {
    expect_is(results$pr_curve[[name]], "happy_roc")
  }
})

test_that("hap.py version is detected if runinfo is present", {
  expect_equal(attr(results, "version"), "v0.3.9")
})

test_that("print shows hap.py version", {
  expect_output(print(results), "v0\\.3\\.9")
})

