# happyR
[![Build Status](https://ukch-prd-jnks01.illumina.com/buildStatus/icon?job=happyR/master)](https://ukch-prd-jnks01.illumina.com/job/happyR/job/master/)

Load hap.py results into a useful R data structure

## Install

Download [zip](https://git.illumina.com/bmoore1/happyR/archive/master.zip), extract and run:

```R
devtools::install_local("path/to/happyR-master/")
```

## Usage

```r
library(happyR)

# demo data that comes with the package
happy_input <- system.file("extdata", "happy_demo.summary.csv", package = "happyR")
happy_prefix <- sub(".summary.csv", "", happy_input)

# happy_prefix is the -o argument to hap.py, here: path/to/files/happy_demo
hapdata <- read_happy(happy_prefix)
hapdata
# Hap.py result containing:  summary, extended, pr_curve 
# 
# # happy_summary [4 × 16]
# Type Filter TRUTH.TOTAL TRUTH.TP TRUTH.FN QUERY.TOTAL QUERY.FP QUERY.UNK FP.gt METRIC.Recall
# <chr>  <chr>       <int>    <int>    <int>       <int>    <int>     <int> <int>         <dbl>
# 1 INDEL    ALL        8946     7840     1106       11811      367      3494    46      0.876369
# 2 INDEL   PASS        8946     7551     1395        9970      301      1944    31      0.844064
# 3   SNP    ALL       52494    52125      369       90092      548     37383   107      0.992971
# 4   SNP   PASS       52494    46920     5574       48078      122      1014     8      0.893816
# # ... with 6 more variables: METRIC.Precision <dbl>, METRIC.Frac_NA <dbl>, TRUTH.TOTAL.TiTv_ratio <dbl>,
# #   QUERY.TOTAL.TiTv_ratio <dbl>, TRUTH.TOTAL.het_hom_ratio <dbl>, QUERY.TOTAL.het_hom_ratio <dbl>
```

`hapdata` is now a single hap.py data object containing:
* `summary` (from summary.csv) - high-level ALL / PASS numbers
* `extended` (from extended.csv) - region / subtype stratified metrics
* `pr_curve` (from roc.*.csv.gz) - precision-recall over quality score

```r
names(hapdata)
# [1] "summary"  "extended" "pr_curve"

# e.g. here pr_curve$INDEL_PASS maps to happy_demo.roc.Locations.INDEL.PASS.csv.gz
names(hapdata$pr_curve)
# [1] "all"        "INDEL"      "INDEL_PASS" "INDEL_SEL"  "SNP"        "SNP_PASS"   "SNP_SEL"   
```

## Example plots

### Indel subtypes
```r
## get indel subtypes from 'extended', skipping complex alleles and combined
indel_extended <- subset(hapdata$extended, Type == "INDEL" & Filter == "ALL" & grepl("^[DI]", Subtype))

# Precision-recall by subtype, scaled by number in truthset
ggplot(indel_extended, aes(x=METRIC.Recall, y=METRIC.Precision, col=Subtype, size=TRUTH.TOTAL)) +
  geom_point() + theme_minimal() + 
  scale_color_brewer(palette = "Set2") +
  scale_size(guide = "none") +
  ggtitle("Indel subtype precision and recall")
```
<img src="https://git.illumina.com/bmoore1/happyR/raw/master/examples/happyr_eg_indelext.png" width="600">


### Precision-recall curves
```r
# PR curve starting at ALL point
all_pr <- subset(hapdata$pr_curve$all, Filter == "ALL" & Subtype == "*")

ggplot(all_pr, aes(x=METRIC.Recall, y=METRIC.Precision, col=Type)) +
  geom_line() + theme_minimal() +
  geom_point(data=hapdata$summary) +
  scale_x_continuous(limits = c(.6, 1)) +
  scale_y_continuous(limits = c(.95, 1)) +
  ggtitle("ALL PR curve might not hit the PASS point")
```


```r
# selectively filtered PR curve
pr <- subset(hapdata$pr_curve$all, Filter == "SEL" & Subtype == "*")

# link this to the ALL point
pr <- dplyr::bind_rows(pr, subset(hapdata$summary, Filter == "ALL"))

ggplot(pr, aes(x=METRIC.Recall, y=METRIC.Precision, col=Type)) +
  geom_line() + theme_minimal() +
  geom_point(data=hapdata$summary) +
  scale_x_continuous(limits = c(.6, 1)) +
  scale_y_continuous(limits = c(.95, 1)) +
  ggtitle("Selectively-filtered PR curve reliably hits PASS")

```
