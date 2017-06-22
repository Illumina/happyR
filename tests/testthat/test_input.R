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


context("User feedback")

test_that("quietly suppresses loading messages", {
  expect_silent(results <- read_happy(file.path(dirname(dir), "happy_demo"), quietly = TRUE))
})

test_that("without quietly user gets loading feedback", {
  expect_message(results <- read_happy(file.path(dirname(dir), "happy_demo"), quietly = FALSE))
})

test_that("error messages distinguish missing directories from non-hap.py results", {
  expect_error(read_happy("/a/bad/dir"), regexp = "directory")
  expect_error(read_happy("/bin/sh"), regexp = "prefix")
})

test_that("read_happy compains about partial hap.py results", {
  file.create("test.summary.csv")
  expect_error(read_happy("test"), "Missing expected hap.py output file: test.extended.csv")
  unlink("test.summary.csv")
})
