context("Lazy loading")

# setup :: load input data bundled with package
dir <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
stopifnot(nchar(dir) > 1)
suppressMessages(results <- read_happy(file.path(dirname(dir), "happy_demo")))

suppressMessages(results_nonlazy <- read_happy(file.path(dirname(dir), "happy_demo"), lazy = FALSE))


test_that("PR data are lazy-loaded if selected", {
  expect_true(pryr:::is_promise2("all", results$pr_curve))
  expect_false(pryr:::is_promise2("all", results_nonlazy$pr_curve))
})

test_that("Lazy data is properly forced", {
  pr_df <- results$pr_curve$INDEL_PASS
  expect_is(pr_df, "happy_roc")
  expect_true(nrow(pr_df) > 10)
})
