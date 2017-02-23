#!/bin/bash

# point to hap.py install
HAP=/illumina/development/haplocompare/haplocompare-v0.3.6/bin/hap.py

$HAP ../extdata/PG_NA12878_chr21.vcf.gz ../extdata/NA12878-GATK3-chr21.vcf.gz \
     -f ../extdata/PG_Conf_chr21.bed.gz --roc QUAL --roc-filter LowQual \
     -o happy_demo --no-json
