## Cleaner access to hap.py data
pr_data <- function(happy_result, filter = c("ALL", "PASS", "SEL"),
                    start_from = c("ALL", "PASS"),
                    selectively_filter = c(TRUE, FALSE),
                    subtype = c("*", "C16_PLUS", "C1_5", "C6_15", "D16_PLUS",
                                "D1_5", "D6_15", "I16_PLUS", "I1_5", "I6_15"),
                    subset = NULL) {

  if (class(happy_result) != "happy_result"){
    stop("Object must be a happy_result loaded via happyR, ",
         "not a ", class(happy_result))
  }

  # TODO
  filter <- match.arg(filter)
  subtype <- match.arg(subtype, several.ok = TRUE)

  if (!is.null(subset)){
    # filter by subset, warn if fails
  }

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
