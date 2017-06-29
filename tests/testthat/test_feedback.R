context("User feedback")

# setup :: load input data bundled with package
dir <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
suppressMessages(results <- read_happy(file.path(dirname(dir), "happy_demo")))

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

test_that("extract_results rejects non happy_result_lists", {
  expect_error(extract_results(list(a = 1, b = 2)))
  expect_error(extract_results(results, "summary"))
})

test_that("pr_data rejects non happy_results", {
  expect_error(pr_data(list(a = "pr.all")), "list")
  expect_error(pr_data(1:10), "integer")
})
