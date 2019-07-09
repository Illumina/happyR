## generics

#' Print a hap.py results object
#'
#' Pretty-print the contents of a list-style
#' object containing hap.py result data
#'
#' @param x an object of class happy_result
#' @param ... additional args passed to \code{tibble::trunc_mat}
#'
#' @examples
#'
#' \dontrun{
#' # not run example:
#' happy <- read_happy('/output/path/prefix')
#' print(happy)
#' }
#'
#' # running example with package demo data
#' happy_input <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
#' happy_prefix <- sub(".summary.csv", "", happy_input)
#'
#' # load happy result
#' hapdata <- read_happy(happy_prefix)
#' print(hapdata)
#'
#' @seealso \code{\link[tibble]{trunc_mat}}
#'
#' @export
print.happy_result <- function(x, ...){

  # show contents of object (i.e. which hap.py files loaded)
  present <- names(x)[which(!vapply(x, is.null, logical(1)))]
  cat("  Hap.py result containing: ", paste(present, collapse=", "), "\n",
      "  Loaded from: ", attr(x, "happy_prefix"))
  version <- attr(x, "version")
  if (version != "Unknown") {
    cat("  (hap.py version: ", version, ")", sep = "")
  }
  cat("\n\n")

  # simplified results summary (drop some columns)
  print(tibble::trunc_mat(x$summary, ...))

  invisible()
}

#' Combine multiple hap.py results objects
#'
#' Combine >1 objects of class \code{happy_result} into
#' a list, or extend an existing \code{happy_result_list}.
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
#' @return a list of \code{happy_result} objects
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

  out_names <- lapply(out_list, attr, "happy_prefix")
  if (length(unique(out_names)) < length(out_names))
    warning("Combining redundant hap.py results")

  structure(out_list, class="happy_result_list")
}


#' @rdname c.happy_result
#' @export
c.happy_result_list <- function(...){
  c.happy_result(...)
}
