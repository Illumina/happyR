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
  cat("  Hap.py result containing: ", paste(present, collapse=", "), "\n\n")

  # simplified results summary (drop some columns)
  print(tibble::trunc_mat(happy_result$summary))

  return(invisible())
}

