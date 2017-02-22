## Load hap.py data


#' Load a hap.py results directory
#'
#' Read a directory of hap.py output files into
#' a R data structure.
#'
#' @param happy_prefix hap.py output prefix (and path)
#' @param lazy store lesser-used output as
#'   unevaluated promises rather than reading
#'   everything at once
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
#' names(happy)
#' }
#'
#' @export
read_happy <- function(happy_prefix, lazy = TRUE){

}
