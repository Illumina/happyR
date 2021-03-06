% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{c.happy_result}
\alias{c.happy_result}
\alias{c.happy_result_list}
\title{Combine multiple hap.py results objects}
\usage{
\method{c}{happy_result}(...)

\method{c}{happy_result_list}(...)
}
\arguments{
\item{...}{Multiple \code{happy_result} objects}
}
\value{
a list of \code{happy_result} objects
}
\description{
Combine >1 objects of class \code{happy_result} into
a list, or extend an existing \code{happy_result_list}.
}
\examples{

\dontrun{
happy1 <- read_happy('/output/path/prefix')
happy2 <- read_happy('/another/path/prefix')

happy_results <- c(happy1, happy2)
class(happy_results)
# happy_results_list
}

}
