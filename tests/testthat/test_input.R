## Input testing

context("Load hap.py results")

# read_happy
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

# read_samplesheet_

extdata_dir <- system.file("extdata", package = "happyR")
samplesheet <- readr::read_csv("group_id,replicate_id,happy_prefix
PCR-Free,NA12878-I30,NA12878-I30_S1
PCR-Free,NA12878-I33,NA12878-I33_S1
Nano,NA12878-R1,NA12878-R1_S1
Nano,NA12878-R2,NA12878-R2_S1
")
samplesheet$happy_prefix <- sprintf("%s/%s", extdata_dir, samplesheet$happy_prefix)

test_that("read_samplesheet_ loads data with no errors and returns the expected happy_samplesheet object", {

  expect_error(happy_samplesheet <- read_samplesheet_(samplesheet, lazy = TRUE), NA)

  expect_is(happy_samplesheet, "happy_samplesheet")
  expect_true(all(names(happy_samplesheet) %in% c("samplesheet", "results")))

  s <- happy_samplesheet$samplesheet
  expect_is(s, "data.frame")
  expect_equal(dim(s)[1], 4)
  expect_equal(dim(s)[2], 5)

  r <- happy_samplesheet$results
  expect_is(r, "happy_result_list")
  expect_equal(length(r), 4)

  expect_is(r[[1]], "happy_result")
  expect_true(all(names(r[[1]]) %in% c("summary", "extended", "pr_curve")))

  e <- happyR::extract_results(r, table = "summary")
  expect_is(e, "data.frame")
  expect_equal(dim(e)[1], 16)
  expect_equal(dim(e)[2], 18)
  expect_true("from" %in% colnames(e))

  e <- happyR::extract_results(r, table = "extended")
  expect_is(e, "data.frame")
  expect_equal(dim(e)[1], 704)
  expect_equal(dim(e)[2], 66)
  expect_true("from" %in% colnames(e))

  p <- r[[1]]$pr_curve
  expect_is(p, "environment")
  expect_true(all(names(p) %in% c("INDEL_SEL", "INDEL_PASS", "INDEL", "all")))

  e <- happyR::extract_results(r, table = "pr.indel.pass")
  expect_is(e, "data.frame")
  expect_equal(dim(e)[1], 9126)
  expect_equal(dim(e)[2], 67)
  expect_true("from" %in% colnames(e))

  expect_is(b[[1]], "data.frame")
  expect_equal(dim(b[[1]])[1], 1)
  expect_equal(dim(b[[1]])[2], 136)

})
