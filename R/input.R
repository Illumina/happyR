## Load hap.py data


#' Load a hap.py results directory
#'
#' Read a directory of hap.py output files into
#' a R data structure
#'
#' @param directory path to the output directory
#' @param lazy store lesser-used output as
#'   unevaluated promises rather than reading
#'   everything
#'
#' @return A list structure containing hap.py output
#'
#' @examples
#'
#' \dontrun{
#' happy <- read_happy('/path/to/output/dir')
#' names(happy)
#' }
#'
#' @export
read_happy <- function(directory, lazy = TRUE){

}
