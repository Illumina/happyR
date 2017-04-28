## Cleaner access to hap.py data
pr_data <- function(happy_result,
                    var_type = "both",
                    filter = c("ALL", "PASS", "SEL"),
                    start_from = c("ALL", "PASS"),
                    selectively_filter = c(TRUE, FALSE),
                    subtype = c("*", "C16_PLUS", "C1_5", "C6_15", "D16_PLUS",
                                "D1_5", "D6_15", "I16_PLUS", "I1_5", "I6_15"),
                    subset = NULL) {

  if (class(happy_result) != "happy_result"){
    stop("Object must be a happy_result loaded via happyR, ",
         "not a ", class(happy_result))
  }

  filter <- match.arg(filter)
  var_type <- match.arg(var_type, choices = c("both", "snv", "indel"))
  subtype <- match.arg(subtype, several.ok = TRUE)

  # starting point: smallest possible PR file
  outdf <- if (filter == "ALL" | !is.null(subset) | var_type == "both") {
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
  message(nrow(outdf), " records loaded")

  if (!is.null(subset)){
    # filter by subset, warn if fails
    outdf <- outdf[outdf$Subset %in% subset]
    if (!nrow(outdf) > 0){
      warning("No PR data found for subset: ", subset)
    }
  }

  outdf <- outdf[outdf$Subtype %in% subtype & outdf$Filter %in% filter,]

  return(outdf)
}


# summary of factor levels in PR data.frame
pr_describe <- function(pr_data) {

  for (col_index in 1:ncol(pr_data)) {
    # summarise column contents (if factor-like)
    col_summary <- if (class(pr_data[[col_index]]) %in% c("character", "factor")) {
      members <- sort(unique(as.character(pr_data[[col_index]])))
      if (length(members) > 10) {
        colstring <- paste(c(members[1:9], "..."), collapse = ", ")
      } else {
        colstring <- paste(members, collapse = ", ")
      }
    } else {
      colstring <- paste(c("min", "lQR", "median", "uQR", "max"),
                         signif(fivenum(pr_data[[col_index]]), digits = 3),
                         sep = ":")
      colstring <- paste(colstring, collapse = ", ")
    }
    cat("  ", colnames(pr_data)[col_index], ": ",
        "<", class(pr_data[[col_index]]), "> ",
        colstring, "\n", sep = "")
  }

  invisible()
}

#' Extract tables from hap.py result lists
#'
#' Extract tables from multiple hap.py result objects and combine
#' into a single \code{data.frame}. Source information from each
#' result is added as an additional column (\code{from}).
#'
#' @param happy_result_list A \code{happy_result_list} object, created
#'   by combining \code{happy_result}s together with \code{c}
#' @param table Table of data to extract from each result
#'
#' @examples
#'
#' \dontrun{
#' happy1 <- read_happy('/output/path/prefix')
#' happy2 <- read_happy('/different/path/prefix')
#' results_list <- c(happy1, happy2)
#'
#' # get full extended metrics for all results as a data.frame
#' extended_df <- extract(results_list, table = "extended")
#'
#' # get collapsed summary table of high-level metrics
#' summary_df <- extract(results_list, table = "summary")
#' unique(summary_df$from)
#' # [1] "/output/path/prefix"  "/different/path/prefix"
#' }
#'
#' @export
extract <- function(happy_result_list, table = c("summary", "extended")) {
  # validate input
  if (!"happy_result_list" %in% class(happy_result_list)) {
    stop("Must provide a happy_result_list object.")
  }

  table <- match.arg(table)

  # extract results into a data.frame
  item_list <- lapply(happy_result_list, function(d) {
    if (!table %in% names(d)) {
      stop("Could not find ", table, " in happy_result_list")
    }
    table_out <- d[[table]]
    table_out$from <- attr(d, "from")
    table_out
  })
  df <- dplyr::bind_rows(item_list)

  # set class
  if (table == "summary") {
    class(df) <- c("happy_summary", class(df))
  }
  if (table == "extended") {
    class(df) <- c("happy_extended", class(df))
  }

  return(df)
}
