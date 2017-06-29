context("Extracting tables")

# setup :: load input data bundled with package
dir <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
stopifnot(nchar(dir) > 1)
suppressMessages(results <- read_happy(file.path(dirname(dir), "happy_demo")))

# shared result list
other_results <- results
attr(other_results, "from") <- "dummy/path/to/happy"
result_list <- c(results, other_results)

test_that("summary tables can be extracted from list of happy_result", {
  summary_table <- extract_results(result_list, "summary")
  expect_is(summary_table, "data.frame")
  expect_equal(nrow(summary_table), 8)
})

test_that("pr data can be extracted from lists of happy_results", {
  pr_all <- extract_results(result_list, "pr.all")
  expect_is(pr_all, "data.frame")
  expect_gt(nrow(pr_all), 50)

  pr_snv_pass <- extract_results(result_list, "pr.snp.pass")
  expect_equal(unique(pr_snv_pass$Filter), "PASS")
  expect_equal(unique(pr_snv_pass$Type), "SNP")
  expect_length(unique(pr_snv_pass$from), 2)
  expect_equal(nrow(pr_snv_pass), nrow(results$pr_curve$SNP_PASS) * 2)

  pr_indel_sel <- extract_results(result_list, "pr.indel.sel")
  expect_equal(unique(pr_indel_sel$Filter), "SEL")
  expect_equal(unique(pr_indel_sel$Type), "INDEL")
  expect_length(unique(pr_indel_sel$from), 2)
  expect_equal(nrow(pr_indel_sel), nrow(results$pr_curve$INDEL_SEL) * 2)

  pr_indel_all <- extract_results(result_list, "pr.indel.all")
  expect_equal(unique(pr_indel_all$Filter), "ALL")
  expect_equal(unique(pr_indel_all$Type), "INDEL")
  expect_length(unique(pr_indel_all$from), 2)
  expect_equal(nrow(pr_indel_all), nrow(results$pr_curve$INDEL) * 2)

  pr_snv_all <- extract_results(result_list, "pr.snp.all")
  expect_equal(unique(pr_snv_all$Filter), "ALL")
  expect_equal(unique(pr_snv_all$Type), "SNP")
  expect_length(unique(pr_snv_all$from), 2)
  expect_equal(nrow(pr_snv_all), nrow(results$pr_curve$SNP) * 2)
})

test_that("missing pr data gives a warning on extraction", {

  rm("INDEL_SEL", pos = result_list[[1]]$pr_curve,
     inherits = FALSE)

  expect_error(
    suppressWarnings(pr_sel <- extract_results(result_list, "pr.indel.sel"))
  )

})

test_that("pr_data gets correct PR subsets", {

  # example from vignette
  short_ins1 <- subset(results$pr_curve$all, Filter == "ALL" & Subset == "*" & Subtype == "D1_5")
  short_ins2 <- pr_data(results, var_type = "indel", subtype = "D1_5")
  expect_equal(short_ins1, short_ins2)

})
