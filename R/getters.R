#' Extract hap.py Precision-Recall data
#'
#' Simpler interface to retrieve a data.frame
#' of PR metrics from a happy_result object.
#'
#' @param happy_result a happy result loaded
#'   via \code{\link[happyR]{read_happy}}
#' @param var_type subset for either insertions
#'   and deletions \code{"indel"}, SNVs \code{"snv"}
#'   or keep both
#' @param filter include all records (ALL), only
#'   passing (PASS) or with selective filters applied
#'   (SEL)
#' @param subtype variant subtype of the form \code{[IDC]length_range},
#'   e.g. \code{"D6_15"} is deletions of length \eqn{>=5} and \eqn{<=15}
#' @param subset when run with stratification regions, the subset is
#'   the region ID. \code{"*"} for genome-wide PR data. See details.
#' @param quietly suppress info messages
#'
#' @details
#'
#' \strong{Subsets}: hap.py v0.3.7+ writes subsets \code{TS_contained} and
#' \code{TS_boundary} by default, corresponding to truth variants
#' well contained or at the boundary of confident regions. In some
#' truthsets, those in \code{TS_boundary} will show worse performance
#' metrics due to issues with variant representation or a partial
#' haplotype description.
#'
#' \strong{Subtypes}: Insertion subtypes are of the form: \code{[IDC]length_range}
#' where the first letter indicates the variant classification: \code{I} insertion;
#' \code{D} deletion; and \code{C} complex. Hap.py bins the lengths of these records
#' into ranges by ALT allele length in basepairs: \code{1_5}, \code{6_15} and \code{16_PLUS}.
#'
#' @return a \code{data.frame} of Precision-Recall metrics for the
#'   selected subset
#'
#' @examples
#'
#' # figure out prefix from pkg install location
#' happy_input <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
#' happy_prefix <- sub(".summary.csv", "", happy_input)
#'
#' # load happy result
#' hapdata <- read_happy(happy_prefix)
#'
#' # long deletion PR curve
#' del_pr <- pr_data(hapdata, var_type = "indel", subtype = "D16_PLUS")
#'
#' @export
pr_data <- function(happy_result,
                    var_type = c("both", "snv", "indel"),
                    filter = c("ALL", "PASS", "SEL"),
                    subtype = c("*", "C16_PLUS", "C1_5", "C6_15", "D16_PLUS",
                                "D1_5", "D6_15", "I16_PLUS", "I1_5", "I6_15"),
                    subset = "*",
                    quietly = TRUE) {

  if (class(happy_result) != "happy_result") {
    stop("Object must be a happy_result loaded via happyR, ",
         "not ", class(happy_result))
  }

  filter <- match.arg(filter)
  var_type <- match.arg(var_type)
  if (!missing(subtype)) {
    subtype <- match.arg(subtype, several.ok = TRUE)
  } else {
    # pick first, i.e. '*'
    subtype <- match.arg(subtype)
  }

  # starting point: smallest possible PR file
  outdf <- if (filter == "ALL" | var_type == "both") {
    happy_result$pr_curve$all
  } else if (filter == "SEL") {
    if (var_type == "snv") {
      happy_result$pr_curve$SNP_SEL
    } else {
      happy_result$pr_curve$INDEL_SEL
    }
  } else {
    if (var_type == "indel") {
      happy_result$pr_curve$SNP_PASS
    } else {
      happy_result$pr_curve$INDEL_PASS
    }
  }

  if (!quietly){
    message(nrow(outdf), " records loaded")
  }

  # filter var_type for all
  if (filter == "ALL" & var_type != "both") {
    if (var_type == "snv") {
      outdf <- outdf[outdf$Type != "INDEL",]
    } else {
      outdf <- outdf[outdf$Type == "INDEL",]
    }
  }

  outdf <- outdf[outdf$Subset %in% subset,]
  if (!nrow(outdf) > 0){
    warning("No PR data found for subset: ", subset)
  }

  outdf <- outdf[outdf$Subtype %in% subtype & outdf$Filter %in% filter,]

  if (!quietly) {
    message("subset contains ", nrow(outdf), " records")
  }

  outdf
}

#' Extract tables from hap.py result lists
#'
#' Extract results tables from multiple hap.py result objects and combine
#' into a single \code{data.frame}. Source information from each
#' result is added as an additional column (\code{happy_prefix}).
#'
#' @param happy_result_list A \code{happy_result_list} object.
#' @param table Table of data to extract from each result.
#'   \code{"summary"} or \code{"extended"} get top level tables;
#'   the \code{pr} options get Precision-Recall tables.
#'
#' @return a \code{data.frame} of combined tables from list
#'
#' @examples
#'
#' \dontrun{
#' samplesheet <- readr::read_csv("group_id,replicate_id,happy_prefix
#' PCR-Free,NA12878-I30,NA12878-I30_S1
#' PCR-Free,NA12878-I33,NA12878-I33_S1
#' Nano,NA12878-R1,NA12878-R1_S1
#' Nano,NA12878-R2,NA12878-R2_S1
#' ")
#' hap_samplesheet <- read_samplesheet_(samplesheet = samplesheet_df)
#'
#' # get collapsed summary table of high-level metrics
#' summary_df <- extract_results(hap_samplesheet$results, table = "summary")
#' unique(summary_df$happy_prefix)
#' # [1] "/output/path/prefix"  "/different/path/prefix"
#' }
#'
#' @export
extract_results <- function(happy_result_list,
                    table = c("summary", "extended",
                              "pr.all",
                              "pr.indel.pass", "pr.indel.sel", "pr.indel.all",
                              "pr.snp.pass", "pr.snp.sel", "pr.snp.all")) {
  # validate input
  if (!"happy_result_list" %in% class(happy_result_list)) {
    stop("Must provide a happy_result_list object.")
  }

  table <- match.arg(table)

  if (grepl("^pr\\.", table)) {

    if (grepl("all", table)) {
      if (table == "pr.all") {
        path <- "all"
      } else {
        # pr.indel.all -> INDEL
        path <- sub(".*?\\.([[:alpha:]]*?)\\..*$", "\\U\\1\\E", table, perl = TRUE)
      }
    } else {
      # reformat + convert to uppercase, e.g.: pr.snp.pass -> "SNP_PASS"
      path <- sub(".*?\\.([[:alpha:]]*?)\\.([[:alpha:]]*$)", "\\U\\1_\\2\\E", table, perl = TRUE)
    }

    item_list <- lapply(happy_result_list, function(d) {

      if (!exists(path, envir = d$pr_curve, inherits = FALSE)) {
        warning("missing pr data: ", path,
                " in R object from: ", attr(d, "happy_prefix"),
                " - skipping", call. = FALSE)
        return (NULL)
      }

      table_out <- d$pr_curve[[path]]
      if (is.null(table_out)) {
        warning("missing pr data: ", path,
                " in R object from: ", attr(d, "happy_prefix"),
                " - skipping", call. = FALSE)
        return (NULL)
      }
      table_out$happy_prefix <- attr(d, "happy_prefix")
      table_out
    })

  } else {
    # not PR data, e.g. summary / extended

    item_list <- lapply(happy_result_list, function(d) {
      if (!table %in% names(d)) {
        stop("Could not find ", table, " in happy_result_list")
      }
      table_out <- d[[table]]
      table_out$happy_prefix <- attr(d, "happy_prefix")
      table_out
    })

  }

  df <- dplyr::bind_rows(item_list)
  if (nrow(df) == 0) {
    stop("no results found for extraction")
  }

  # set class
  if (table == "summary") {
    class(df) <- c("happy_summary", class(df))
  }
  if (table == "extended") {
    class(df) <- c("happy_extended", class(df))
  }

  df
}
