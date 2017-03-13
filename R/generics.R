## generics

#' Print a hap.py results object
#'
#' Pretty-print the contents of a list-style
#' object containing hap.py result data
#'
#' @param happy_result an object of class happy_result
#' @param ... additional args passed down to \code{tibble::trunc_mat}
#'
#' @examples
#'
#' \dontrun{
#' happy <- read_happy('/output/path/prefix')
#' print(happy)
#' }
#' @seealso tibble::trunc_mat
#'
#' @export
print.happy_result <- function(happy_result, ...){

  # show contents of object (i.e. which hap.py files loaded)
  present <- names(happy_result)[which(!vapply(happy_result, is.null, logical(1)))]
  cat("  Hap.py result containing: ", paste(present, collapse=", "), "\n",
      "  Loaded from ", attr(happy_result, "from"), "\n\n")

  # simplified results summary (drop some columns)
  print(tibble::trunc_mat(happy_result$summary))

  return(invisible())
}

#' Combine multiple hap.py results objects
#'
#' Combine >1 objects of class "happy_result" into
#' a list, or extend an existing "happy_result_list".
#'
#' @param ... Multiple \code{happy_result} objects
#'
#' @examples
#'
#' \dontrun{
#' happy1 <- read_happy('/output/path/prefix')
#' happy2 <- read_happy('/another/path/prefix')
#'
#' happy_results <- c(happy1, happy2)
#' class(happy_results)
#' # happy_results_list
#' }
#'
#' @export
c.happy_result <- function(...){

  to_c <- as.list(substitute(list(...)))[-1L]
  classes <- lapply(to_c, class)

  out_list <- list()
  out_names <- c()
  recognised_classes <- c("happy_result", "happy_result_list")

  if (!all(classes %in% recognised_classes)){
    stop("Cannot combine happy_result with non-happy_result")
  }

  lists <- which(classes == "happy_result")
  if (length(lists) > 0){
    out_list <- to_c[lists]
  }

  # unpack existing list of lists
  list_of_lists <- which(classes == "happy_result_list")
  if (length(list_of_lists) > 0) {
    for (l in list_of_lists) {
      this_list <- to_c[[l]]
      stopifnot(class(this_list) == "happy_result_list")
      for (sublist in seq_along(this_list)) {
        stopifnot(class(this_list[[sublist]]) == "happy_result")
        out_list <- append(out_list, this_list[sublist])
      }
    }
  }

  out_names <- lapply(out_list, attr, "from")
  if (length(unique(out_names)) < length(out_names))
    warning("Combining redundant hap.py results")

  return(structure(out_list, class="happy_result_list"))
}


#' @rdname c.happy_result
#' @export
c.happy_result_list <- function(...){
  c.happy_result(...)
}


#' @export
extract = function(x, ...) UseMethod("extract")


#' Extract items from happy result lists
#'
#' Extract items across multiple happy result objects and combine into a single `data.frame`.
#'
#' @param happy_result_list A `happy_result_list` object.
#' @param item Item to extract. One of: summary, extended.
#'
#' @export
extract.happy_result_list = function(happy_result_list, item, ...) {
  # validate input
  if (!class(happy_result_list)[1] == "happy_result_list") {
    stop("Must provide a happy_result_list object.")
  }

  if (!item %in% c("summary", "extended")) {
    stop("Invalid item selected")
  }

  # extract results into a data.frame
  item_list = lapply(happy_result_list, function(d) {
    if (!item %in% names(d)) {
      stop("Could not find item in happy_result_list")
    }
    d[[item]]
  })
  df = item_list %>% dplyr::bind_rows()

  # set class
  if (item == "summary") {
    class(df) <- c("happy_summary", class(df))
  }
  if (item == "extended") {
    class(df) <- c("happy_extended", class(df))
  }

  return(df)
}
