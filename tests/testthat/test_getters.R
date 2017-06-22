context("Extracting tables")

# setup :: load input data bundled with package
dir <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
stopifnot(nchar(dir) > 1)
suppressMessages(results <- read_happy(file.path(dirname(dir), "happy_demo")))


test_that("summary tables can be extracted from list of happy_result", {
  other_results <- results
  attr(other_results, "from") <- "dummy/path/to/happy"
  result_list <- c(results, other_results)
  summary_table <- extract(result_list, "summary")

  expect_is(summary_table, "data.frame")
  expect_equal(nrow(summary_table), 8)
})

test_that("pr_data gets correct PR subsets", {

  # example from vignette
  short_ins1 <- subset(results$pr_curve$all, Filter == "ALL" & Subset == "*" & Subtype == "D1_5")
  short_ins2 <- pr_data(results, var_type = "indel", subtype = "D1_5")
  expect_equal(short_ins1, short_ins2)

})
