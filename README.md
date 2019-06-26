
# happyR

[![Build
Status](https://travis-ci.org/Illumina/happyR.svg?branch=master)](https://travis-ci.org/Illumina/happyR)
[![codecov](https://codecov.io/gh/Illumina/happyR/branch/master/graph/badge.svg)](https://codecov.io/gh/Illumina/happyR)

Load [hap.py](https://github.com/Illumina/hap.py) results into an R data
structure to enable simple plotting, comparisons and aggregation.

## Install

``` r
devtools::install_github("Illumina/happyR")
```

## Usage

``` r
# set up
library(happyR)
library(tidyverse, quietly = TRUE)


# define happyr samplesheet
extdata_dir <- system.file("extdata", package = "happyR")
samplesheet <- tibble::tribble(
~group_id, ~replicate_id, ~happy_prefix,
"PCR-Free", "NA12878-I30", "NA12878-I30_S1",
"PCR-Free", "NA12878-I33", "NA12878-I33_S1",
"Nano", "NA12878-R1", "NA12878-R1_S1",
"Nano", "NA12878-R2", "NA12878-R2_S1"
) %>% 
mutate(happy_prefix = sprintf("%s/%s", extdata_dir, happy_prefix))


# load hap.py results
hap_samplesheet <- read_samplesheet_(samplesheet)


# query and visualise performance metrics
summary <- extract_results(hap_samplesheet$results, table = "summary") %>% 
  inner_join(samplesheet, by = "happy_prefix") %>% 
  filter(Filter == "PASS")
  
ggplot(data = summary, aes(x = METRIC.Recall, y = METRIC.Precision, color = group_id, shape = Type)) +
  geom_point() +
  xlim(NA, 1) +
  ylim(NA, 1) +
  theme_minimal() + 
  scale_color_brewer(palette = "Set2") +
  scale_size(guide = "none") +
  ggtitle("Precision-Recall scatterplot")  
```

<img src="examples/README-usage-1.png" style="display: block; margin: auto;" />

## System requirements

Originally developed for R v3.4.0.
[Tests](https://travis-ci.org/Illumina/happyR) are run using the most
recent available R versions (incl. devel) on Ubuntu (Trusty) and OS X
(El Capitan) platforms. happyR has not been tested on Windows.
Dependencies are listed in [DESCRIPTION](DESCRIPTION).
