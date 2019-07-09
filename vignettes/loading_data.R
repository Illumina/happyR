## ----opts, echo=FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ----message=FALSE, warning=FALSE----------------------------------------
library(happyR)
library(tidyverse)

## ------------------------------------------------------------------------
# define happy_prefix (the -o argument to hap.py, here: path/to/files/happy_demo)
extdata_dir <- system.file("extdata", package = "happyR")
happy_prefix <- sprintf("%s/happy_demo", extdata_dir)

# load hap.py results
hap_result <- read_happy(happy_prefix)
class(hap_result)
names(hap_result)

## ------------------------------------------------------------------------
hap_result$summary %>% head

hap_result$extended %>% head

names(hap_result$pr_curve)
# e.g. here pr_curve$INDEL_PASS maps to happy_demo.roc.Locations.INDEL.PASS.csv.gz
hap_result$pr_curve$INDEL_PASS %>% head

## ------------------------------------------------------------------------
del_pr <- pr_data(hap_result, var_type = "indel", filter = "PASS", subtype = "*")
del_pr %>% head

## ----warning=FALSE-------------------------------------------------------
# define happyr samplesheet
extdata_dir <- system.file("extdata", package = "happyR")
samplesheet <- readr::read_csv("group_id,replicate_id,happy_prefix
PCR-Free,NA12878-I30,NA12878-I30_S1
PCR-Free,NA12878-I33,NA12878-I33_S1
Nano,NA12878-R1,NA12878-R1_S1
Nano,NA12878-R2,NA12878-R2_S1
") %>% 
mutate(happy_prefix = sprintf("%s/%s", extdata_dir, happy_prefix))

samplesheet

# load hap.py results
hap_samplesheet <- read_samplesheet_(samplesheet)
# or directly from a samplesheet.csv
# hap_samplesheet <- read_samplesheet(samplesheet_path = "/path/to/happyr_samplesheet.csv")
class(hap_samplesheet)
names(hap_samplesheet)

## ------------------------------------------------------------------------
hap_samplesheet$samplesheet %>% head
hap_samplesheet$ids %>% head

## ------------------------------------------------------------------------
summary <- extract_results(hap_samplesheet$results, table = "summary")
summary %>% head
# see the extract_results documentation for a list of possible values for the table argument

