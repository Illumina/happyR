## Load hap.py data

quiet <- function(...) {
  suppressMessages(suppressWarnings(...))
}

# Generic loader for CSVs written by hap.py
#' @import readr
load_happy_csv <- function(path, class = NULL) {

  if (!file.exists(path)) {
    stop("Missing expected hap.py output file: ", path)
  }

  # extended needs fully-specified cols as some could be
  # NAs for the first 10 - 100,000 or so
  if (class == "happy_extended") {
    dt <- quiet(readr::read_csv(path, progress = FALSE,
                                na = c("", "NA", "."),
                                guess_max = 1e5,
                                col_types = colspec("extended")))

  } else {
    # use readr for speedup, not using fread as we need gzip support
    dt <- quiet(readr::read_csv(path, progress = FALSE,
                                na = c("", "NA", "."),
                                guess_max = 1e4))
  }

  if (!is.null(class)) {
    class(dt) <- c(class, class(dt))
  }

  dt
}

lazy_pr <- function(prefix){
  df <- quiet(readr::read_csv(prefix, progress = FALSE,
                              na = c("", "NA", "."),
                              col_types = colspec("pr"),
                              guess_max = 1e5))
  df$file_source <- prefix
  class(df) <- c("happy_roc", class(df))
  df
}

# Load sets of hap.py Precision-Recall data
load_happy_pr <- function(happy_prefix, lazy_load, quietly) {

  # all potential PR data files that may exist
  possible_suffixes <- paste0(
    c(".roc.all",
      ".roc.Locations.INDEL", ".roc.Locations.INDEL.PASS", ".roc.Locations.INDEL.SEL",
      ".roc.Locations.SNP", ".roc.Locations.SNP.PASS", ".roc.Locations.SNP.SEL"
    ), ".csv.gz")

  if (any(file.exists(paste0(happy_prefix, possible_suffixes)))) {
    if (!quietly) {
      message("Reading precision-recall curve data")
    }

    pr_data <- new.env()
    prefixes <- paste0(happy_prefix, possible_suffixes)

    for (prefix in prefixes) {
      if (file.exists(prefix)) {

        # list component name (basename not needed, happy_prefix contains path)
        this_name <- sub(paste0(happy_prefix, ".roc."), "", prefix)
        this_name <- gsub("\\.", "_", sub(".csv.gz", "", this_name))
        this_name <- sub("Locations_", "", this_name)

        # see also pryr::`%<d-%`
        if (this_name == "all" & lazy_load) {
          all_pref <- substitute(prefix)  # avoids delayed variable eval
          delayedAssign("all", lazy_pr(all_pref), assign.env = pr_data)
        } else {
          assign(substitute(this_name), lazy_pr(prefix), envir = pr_data)
        }

      } else {
        message("Missing file: ", prefix)
      }
    }

  } else {
    # no pr data detected
    if (!quietly)
      message("No precision-recall curve data found")

    pr_data <- NULL
  }

  pr_data
}

# informative error messages that distinguish:
#   * bad file path
#   * good file path that's not a hap.py result
check_happy_path <- function(prefix) {
  test_file <- paste0(prefix, ".summary.csv")
  test_dir <- dirname(normalizePath(prefix, mustWork = FALSE))

  if (!file.exists(test_dir)) {
    stop("Can't read directory: ", test_dir)
  }

  if (!file.exists(test_file)) {
    stop("Summary CSV missing, is ",
         normalizePath(prefix, mustWork = FALSE),
         " a hap.py output prefix?")
  }

  invisible(NULL)
}

# hap.py v0.3.8+ write a JSON of runinfo
get_happy_version <- function(prefix) {

  version <- "Unknown"
  runinfo_json <- paste0(prefix, ".runinfo.json")

  if (file.exists(runinfo_json)) {
    json <- jsonlite::read_json(runinfo_json)
    version <- json$metadata$required$version
  }

  version
}

#' Load a hap.py results directory
#'
#' Read a directory of hap.py output files into
#' an R data structure.
#'
#' @param happy_prefix hap.py output prefix (and path)
#' @param lazy store lesser-used output as
#'   unevaluated promises rather than reading
#'   everything at once
#' @param quietly inhibit logging messages as files
#'   are loaded
#'
#' @return A list structure containing hap.py output
#'
#' @examples
#'
#' \dontrun{
#' # run hap.py from commandline:
#' #  hap.py truth.vcf query.vcf -o /output/path/prefix
#'
#' # load result into R
#' happy <- read_happy('/output/path/prefix')
#' }
#'
#' # load demo data supplied with package
#' happy_input <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
#' happy_prefix <- sub(".summary.csv", "", happy_input)
#'
#' hapdata <- read_happy(happy_prefix)
#' print(hapdata)
#'
#' @export
read_happy <- function(happy_prefix, lazy = TRUE, quietly = FALSE){

  check_happy_path(happy_prefix)

  if (!quietly)
    message("Reading summary table")
  summary <- load_happy_csv(paste0(happy_prefix, ".summary.csv"),
                            "happy_summary")

  if(!quietly)
    message("Reading extended table")
  extended <- load_happy_csv(paste0(happy_prefix, ".extended.csv"),
                             "happy_extended")

  # pr curves -- may or may not be present
  pr_data <- load_happy_pr(happy_prefix, lazy_load = lazy, quietly = quietly)

  # version from runinfo (hap.py v0.3.8+)
  happy_version <- get_happy_version(prefix = happy_prefix)

  happy_result <- structure(
    list(
      summary = summary,
      extended = extended,
      pr_curve = pr_data
    ),
    class = "happy_result",
    happy_prefix = happy_prefix,
    version = happy_version)

  happy_result
}

#' Load hap.py results from a samplesheet csv
#'
#' Load multiple hap.py results into a \code{happy_samplesheet} object starting from a samplesheet csv.
#'
#' @param samplesheet_path Path to samplesheet.csv. Required fields: \code{replicate_id}, \code{happy_prefix}.
#' @param lazy Do not load larger hap.py results until needed. Default: \code{TRUE}.
#'
#' @return A \code{happy_samplesheet} object, with the following fields:
#' \itemize{
#'   \item{\code{samplesheet}: the original samplesheet, stored as a \code{data.frame}}.
#'   \item{\code{results}: a \code{happy_result_list} that contains individual \code{happy_result} objects}.
#' }
#'
#' @examples
#'
#' \dontrun{
#' ## example happyr_samplesheet.csv
#' # group_id,replicate_id,happy_prefix
#' # PCR-Free,NA12878-I30,NA12878-I30_S1
#' # PCR-Free,NA12878-I33,NA12878-I33_S1
#' # Nano,NA12878-R1,NA12878-R1_S1
#' # Nano,NA12878-R2,NA12878-R2_S1
#'
#' hap_samplesheet <- read_samplesheet(samplesheet_path = 'happyr_samplesheet.csv')
#' }
#'
#' @export
read_samplesheet <- function(samplesheet_path, lazy = TRUE) {

  message("Reading happyR samplesheet")
  if (!file.exists(samplesheet_path)) {
    stop(sprintf("Cannot find samplesheet: %s", samplesheet_path))
  }

  samplesheet <- readr::read_csv(samplesheet_path)
  happy_samplesheet <- read_samplesheet_(samplesheet, lazy = lazy)
  return(happy_samplesheet)

}


#' Load hap.py results from a samplesheet dataframe
#'
#' Load multiple hap.py results into a \code{happy_samplesheet} object starting from a samplesheet dataframe.
#'
#' @param samplesheet A \code{happyR} samplesheet (\code{data.frame}). Required fields: \code{replicate_id}, \code{happy_prefix}.
#' @param lazy Do not load larger hap.py results until needed. Default: \code{TRUE}.
#'
#' @return A \code{happy_samplesheet} object, with the following fields:
#' \itemize{
#'   \item{\code{samplesheet}: the original samplesheet, stored as a \code{data.frame}}.
#'   \item{\code{results}: a \code{happy_result_list} that contains individual \code{happy_result} objects}.
#' }
#'
#' @examples
#'
#' \dontrun{
#' samplesheet_df <- tibble::tribble(
#'   ~group_id, ~replicate_id, ~happy_prefix,
#'   "PCR-Free", "NA12878-I30", "NA12878-I30_S1",
#'   "PCR-Free", "NA12878-I33", "NA12878-I33_S1",
#'   "Nano", "NA12878-R1", "NA12878-R1_S1",
#'   "Nano", "NA12878-R2", "NA12878-R2_S1"
#' )
#' hap_samplesheet <- read_samplesheet_(samplesheet = samplesheet_df)
#' }
#'
#' @export
read_samplesheet_ <- function(samplesheet, lazy = TRUE) {

  # validate input
  if (! "data.frame" %in% class(samplesheet)) {
    stop("Samplesheet must be a data.frame")
  }

  required_cols <- c("replicate_id", "happy_prefix")
  if (!all(required_cols %in% colnames(samplesheet))) {
    stop("The provided samplesheet is missing required columns (replicate_id, happy_prefix)")
  }

  # load happy results
  ids <- samplesheet$happy_prefix
  happy_results <- lapply(seq_along(ids), function(i) {
    read_happy(happy_prefix = ids[i], lazy = lazy)
  })
  names(happy_results) <- ids
  class(happy_results) <- c("happy_result_list", class(happy_results))

  # create the happy_samplesheet object
  happy_samplesheet <- list(samplesheet = samplesheet, results = happy_results)
  happy_samplesheet <- structure(happy_samplesheet, class = "happy_samplesheet")

  return(happy_samplesheet)

}
