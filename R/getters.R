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
