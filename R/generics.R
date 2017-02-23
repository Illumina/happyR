## generics

#' Print a hap.py results object
#'
#' Pretty-print the contents of a list-style
#' object containing hap.py result data
#'
#' @examples
#'
#' \dontrun{
#' happy <- read_happy('/output/path/prefix')
#' print(happy)
#' }
#'
#' @export
print.happy_result <- function(happy_result){

  # show contents of object (i.e. which hap.py files loaded)
  cat("  Hap.py result containing: summary, extended", "\n\n")

  # simplified results summary (drop some columns)
  print(tibble::trunc_mat(happy_result$summary))

  return(invisible())
}

