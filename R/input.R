## Load hap.py data

# Generic loader for CSVs written by hap.py
load_happy_csv <- function(path, class = NULL) {
  # using data.table for speedup
  dt <- suppressMessages(suppressWarnings(readr::read_csv(path, progress = FALSE)))
  if (!is.null(class))
    class(dt) <- c(class, class(dt))
  return(dt)
}

# Load sets of hap.py Precision-Recall data
load_happy_pr <- function(happy_prefix, quietly) {

  # all potential PR data files that may exist
  possible_suffixes <- paste0(
    c(".roc.all",
      ".roc.Locations.INDEL", ".roc.Locations.INDEL.PASS", ".roc.Locations.INDEL.SEL",
      ".roc.Locations.SNP", ".roc.Locations.SNP.PASS", ".roc.Locations.SNP.SEL"
    ), ".csv.gz")

  if (file.exists(paste0(happy_prefix, possible_suffixes[1]))) {
    if (!quietly)
      message("Reading precision-recall data")

    pr_data <- list()
    invisible(lapply(paste0(happy_prefix, possible_suffixes), function(fn){
      if (file.exists(fn)){
        # can't use fread on gzipped csv
        this_pr <- suppressWarnings(suppressMessages(readr::read_csv(fn, progress = FALSE)))
        this_pr$file_souce <- fn

        # list component name (basename not needed, happy_prefix contains path)
        this_name <- sub(paste0(happy_prefix, ".roc."), "", fn)
        this_name <- gsub("\\.", "_", sub(".csv.gz", "", this_name))
        this_name <- sub("Locations_", "", this_name)
        pr_data[[this_name]] <<- this_pr
      } else {
        message("Missing file: ", fn)
      }

      NULL
    }))

  } else {
    # no pr data detected
    if (!quietly)
      message("No precision-recall curve data found")

    pr_data <- NULL
  }

  return(pr_data)
}

# som.py pr curves can be more complex than hap.py,
# could be different prefix for SNV / INDEL
load_sompy_pr <- function(sompy_prefix){

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
#' @seealso read_sompy
#'
#' @examples
#'
#' \dontrun{
#' # run hap.py from commandline:
#' #  hap.py truth.vcf query.vcf -o /output/path/prefix
#'
#' # load result into R
#' happy <- read_happy('/output/path/prefix')
#' names(happy)
#' }
#'
#' @export
read_happy <- function(happy_prefix, lazy = TRUE, quietly = FALSE){

  summary_path <- paste0(happy_prefix, ".summary.csv")

  if (!file.exists(summary_path)) {
    stop("File missing -- is ",
         normalizePath(happy_prefix, mustWork = FALSE),
         " a hap.py output prefix?")
  }

  if (!quietly)
    message("Reading summary table")
  summary <- load_happy_csv(summary_path, "happy_summary")

  if(!quietly)
    message("Reading extended table")
  extended <- load_happy_csv(paste0(happy_prefix, ".extended.csv"),
                             "happy_extended")

  # pr curves -- may or may not be present
  pr_data <- load_happy_pr(happy_prefix, quietly = quietly)

  happy_result <- structure(
    list(
      summary = summary,
      extended = extended,
      pr_curve = pr_data
    ),
    class = "happy_result",
    from = happy_prefix)

  return(happy_result)
}

#' Load a som.py results directory
#'
#' Read a directory of som.py (somatic hap.py) output
#' files into an R data structure.
#'
#' @param sompy_prefix som.py output prefix (and path)
#' @param lazy store lesser-used output as
#'   unevaluated promises rather than reading
#'   everything at once
#'
#' @return A list structure containing som.py output
#'
#' @seealso read_happy
#'
#' @examples
#'
#' \dontrun{
#' # run som.py from commandline:
#' #  som.py truth.vcf query.vcf -o /output/path/prefix
#'
#' # load result into R
#' sompy <- read_sompy('/output/path/prefix')
#' names(sompy)
#' }
#'
#' @export
read_sompy <- function(sompy_prefix, lazy = TRUE){

  summary_path <- paste0(sompy_prefix, ".summary.csv")

  if (!file.exists(summary_path)) {
    stop("File missing -- is ",
         normalizePath(sompy_prefix, mustWork = FALSE),
         " a som.py output prefix?")
  }

  summary <- load_happy_csv(summary_path, "happy_summary")
  extended <- load_happy_csv(paste0(sompy_prefix, ".extended.csv"),
                             "happy_extended")

  # pr curves -- may or may not be present
  pr_data <- load_sompy_pr(sompy_prefix)

  happy_result <- structure(
    list(
      summary = summary,
      extended = extended,
      pr_curve = pr_data
    ),
    class = "sompy_result",
    from = sompy_prefix)

  return(happy_result)
}

