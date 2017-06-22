## ----load_settings, message=FALSE----------------------------------------

library(happyR)

happy_input <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
happy_prefix <- sub(".summary.csv", "", happy_input)

hapdata <- read_happy(happy_prefix, quietly = TRUE)

# other packages and options
library(ggplot2)
library(magrittr)
theme_set(theme_minimal())


## ----force_promise, message=FALSE----------------------------------------
# Large 'ALL' PR data isn't loaded yet and the results object isn't too big
if (require(pryr))
  pryr::object_size(hapdata)

## ----head----------------------------------------------------------------
# view part of all PR data, evaluating the promise
knitr::kable(head(hapdata$pr_curve$all[,1:9]))

## ----evald---------------------------------------------------------------
# The hapdata object now includes the full PR data so is a bit bigger
if (require(pryr))
  pryr::object_size(hapdata)

## ----manual_subset-------------------------------------------------------
# subset for short insertions 1 - 5 bp in length
short_ins1 <- subset(hapdata$pr_curve$all, Filter == "ALL" & Subset == "*" & Subtype == "I1_5")

## ----pr_data-------------------------------------------------------------
short_ins2 <- pr_data(hapdata, var_type = "indel", subtype = "I1_5")

# check they give the same results
all.equal(short_ins1, short_ins2)

## ----del_length, fig.align="center", fig.width=5, fig.height=3, warning=FALSE----
# get only deletions for all length ranges
del_pr <- pr_data(hapdata, var_type = "indel", 
                  subtype = c("D1_5", "D6_15", "D16_PLUS"))

ggplot(del_pr, aes(x = METRIC.Recall, y = METRIC.Precision, col = Subtype)) +
  geom_line() + coord_cartesian(ylim = c(.75, 1)) +
  ggtitle("Longer deletions are more difficult to call accurately")

## ----snv_boundary, fig.align="center", fig.width=6, fig.height=3.5, warning=FALSE----
snv_pr <- pr_data(hapdata, var_type = "snv",
                  subset = c("TS_contained", "TS_boundary"))

ggplot(snv_pr, aes(x = METRIC.Recall, y = METRIC.Precision, col = Subset)) +
  geom_line() + coord_cartesian(ylim = c(.8, 1)) +
  ggtitle("TS_contained performance is higher than TS_boundary")

