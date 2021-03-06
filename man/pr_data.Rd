% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getters.R
\name{pr_data}
\alias{pr_data}
\title{Extract hap.py Precision-Recall data}
\usage{
pr_data(happy_result, var_type = c("both", "snv", "indel"),
  filter = c("ALL", "PASS", "SEL"), subtype = c("*", "C16_PLUS",
  "C1_5", "C6_15", "D16_PLUS", "D1_5", "D6_15", "I16_PLUS", "I1_5",
  "I6_15"), subset = "*", quietly = TRUE)
}
\arguments{
\item{happy_result}{a happy result loaded
via \code{\link[happyR]{read_happy}}}

\item{var_type}{subset for either insertions
and deletions \code{"indel"}, SNVs \code{"snv"}
or keep both}

\item{filter}{include all records (ALL), only
passing (PASS) or with selective filters applied
(SEL)}

\item{subtype}{variant subtype of the form \code{[IDC]length_range},
e.g. \code{"D6_15"} is deletions of length \eqn{>=5} and \eqn{<=15}}

\item{subset}{when run with stratification regions, the subset is
the region ID. \code{"*"} for genome-wide PR data. See details.}

\item{quietly}{suppress info messages}
}
\value{
a \code{data.frame} of Precision-Recall metrics for the
  selected subset
}
\description{
Simpler interface to retrieve a data.frame
of PR metrics from a happy_result object.
}
\details{
\strong{Subsets}: hap.py v0.3.7+ writes subsets \code{TS_contained} and
\code{TS_boundary} by default, corresponding to truth variants
well contained or at the boundary of confident regions. In some
truthsets, those in \code{TS_boundary} will show worse performance
metrics due to issues with variant representation or a partial
haplotype description.

\strong{Subtypes}: Insertion subtypes are of the form: \code{[IDC]length_range}
where the first letter indicates the variant classification: \code{I} insertion;
\code{D} deletion; and \code{C} complex. Hap.py bins the lengths of these records
into ranges by ALT allele length in basepairs: \code{1_5}, \code{6_15} and \code{16_PLUS}.
}
\examples{

# figure out prefix from pkg install location
happy_input <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
happy_prefix <- sub(".summary.csv", "", happy_input)

# load happy result
hapdata <- read_happy(happy_prefix)

# long deletion PR curve
del_pr <- pr_data(hapdata, var_type = "indel", subtype = "D16_PLUS")

}
