## ----opts, echo=FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  cache = FALSE,
  comment = "#>")

## ----message=FALSE, warning=FALSE----------------------------------------
library(happyR)
library(tidyverse)
theme_set(theme_minimal())
set.seed(42)

## helper functions for HDI calculation
# source("stratified_counts_util.R")

## ----eval=FALSE----------------------------------------------------------
#  # not run
#  data_dir <- "demo_data.pcrfree_vs_nano/filtered_happy/"
#  samplesheet <- readr::read_csv("group_id,replicate_id,happy_prefix
#  PCR-Free,NA12878-I30,NA12878-I30_S1
#  PCR-Free,NA12878-I33,NA12878-I33_S1
#  PCR-Free,NA12878-I47,NA12878-I47_S1
#  PCR-Free,NA12878-I67,NA12878-I67_S1
#  PCR-Free,NA12878-I82,NA12878-I82_S1
#  PCR-Free,NA12878-I85,NA12878-I85_S1
#  Nano,NA12878-R1,NA12878-R1_S1
#  Nano,NA12878-R2,NA12878-R2_S1
#  Nano,NA12878-R3,NA12878-R3_S1
#  Nano,NA12878-R4,NA12878-R4_S1
#  Nano,NA12878-R5,NA12878-R5_S1
#  Nano,NA12878-R6,NA12878-R6_S1
#  Nano,NA12878-R7,NA12878-R7_S1
#  Nano,NA12878-R8,NA12878-R8_S1
#  ") %>%
#  mutate(happy_prefix = sprintf("%s/%s", data_dir, happy_prefix))
#  
#  hap_samplesheet <- read_samplesheet_(samplesheet)
#  saveRDS(hap_samplesheet, file = "demo_data.Rds")

## ------------------------------------------------------------------------
# load the demo dataset from a pre-saved Rds
hap_samplesheet <- readRDS("demo_data.Rds")
class(hap_samplesheet)

## ------------------------------------------------------------------------
stratified_counts <- extract_results(hap_samplesheet$results, table = "extended") %>% 
  # focus on PASS calls in level 0 subsets
  filter(Subtype == "*", Filter == "PASS", Subset.Level == 0, !grepl(pattern = "TS*", Subset)) %>% 
  inner_join(hap_samplesheet$samplesheet) %>% 
  mutate(.type_group = paste(Type, group_id))

stratified_counts %>% head

## ------------------------------------------------------------------------
stratified_counts %>%
  select(Subset, Type, Subset.Size, TRUTH.TOTAL) %>%
  unique() %>%
  spread(key = Type, value = TRUTH.TOTAL) %>%
  rename(TRUTH.TOTAL.INDEL = INDEL) %>%
  rename(TRUTH.TOTAL.SNP = SNP)

## ----eval=FALSE----------------------------------------------------------
#  # not run
#  groups <- stratified_counts %>% select(.type_group) %>% unique() %>% unlist()
#  demo_hdi <- lapply(seq_along(groups), function(i) {
#    sel_group_type <- groups[i]
#    # initalise model and sample from the posterior distribution
#    stan_result <- sample_posterior(m = stratified_counts %>% filter(.type_group == sel_group_type),
#                                    successes_field = "TRUTH.TP", totals_field = "TRUTH.TOTAL")
#    # calculate 95% HDIs from posterior observations
#    demo_hdi <- estimate_hdi(r = stan_result, credMass = 0.95) %>%
#      mutate(.type_group = sel_group_type)
#    demo_hdi
#  }) %>%
#    bind_rows()
#  
#  demo_hdi %>% head
#  write_csv(demo_hdi, path = "demo_hdi.csv")

## ------------------------------------------------------------------------
# load pre-computed dataset
demo_hdi <- read_csv("demo_hdi.csv", col_types = cols())

## ----fig.width=10, fig.height=5------------------------------------------
dodge_width <- 0.7
demo_hdi %>% 
    ggplot(aes(x = subset, color = .type_group)) +
    # observed
    geom_errorbar(aes(ymin = obs.min, ymax = obs.max), 
                  alpha = 0.2, size = 5, lty = 1, position = position_dodge(width = dodge_width), 
                  width = 0) +
    # estimated
    geom_errorbar(aes(ymin = posterior.hdi.low, ymax = posterior.hdi.high), 
                  size = 0.5, alpha = 1, width = 0.6, position = position_dodge(width = dodge_width)) +
    geom_point(aes(y = posterior.mean), size = 2, shape = 1, 
               position = position_dodge(width = dodge_width)) +
    xlab("") +
    ylab("METRIC.Recall") +
    ylim(0, 1) +
    coord_flip() +
  ggtitle("Recall across genomic subsets in NA12878")

## ----eval=FALSE----------------------------------------------------------
#  # not run
#  
#  # simulate a sample with N subsets, by randomly assigning values to {q_A, q_S, q_N}, x_S and n
#  n_subsets = 1000
#  sim_sample = lapply(1:n_subsets, function(i) {
#      q = runif(3, min = 0, max = 1)
#      q = round(q/sum(q), 4)
#      d = data.frame(
#          i = i,
#          qa = q[1],
#          qs = q[2],
#          qn = q[3]
#      )
#  }) %>%
#    bind_rows() %>%
#    mutate(x = sample(x = seq(0, 1, 1e-4), size = n_subsets, replace = TRUE)) %>%
#    mutate(n = sample(x = seq(1e3, 1e4, 1), size = n_subsets, replace = TRUE)) %>%
#    mutate(expected_rho = round((n*qa + n*qs*x) / n, 4)) %>%
#    mutate(subset_qa_qs_qn_x_n = sprintf("S%s_%s_%s_%s_%s_%s", i, qa, qs, qn, x, n))
#  
#  sim_sample %>% head
#  
#  # simulate r replicates from the sample by drawing counts in each subset, with probabilities {1, x, 0}
#  get_counts <- function(subset_qa_qs_qn_x_n, r) {
#      s <- data.frame(subset_qa_qs_qn_x_n) %>%
#          separate(subset_qa_qs_qn_x_n, into = c("subset", "qa", "qs", "qn", "x", "n"), sep = "_")
#  
#      counts <- table(sample(
#        factor(c("always", "sometimes", "never")), s$n, prob = c(s$qa, s$qs, s$qn), replace = TRUE))
#      always <- counts["always"]
#      sometimes <- rbinom(as.numeric(r), counts["sometimes"], as.numeric(s$x))
#      per_replicate_successes <- data.frame(
#          subset_qa_qs_qn_x_n = subset_qa_qs_qn_x_n,
#          replicate_id = paste("R", 1:r, sep = ""),
#          successes = always + sometimes,
#          stringsAsFactors = FALSE
#      )
#      per_replicate_successes
#  }
#  
#  sim_replicates <- lapply(sim_sample$subset_qa_qs_qn_x_n, function(s) get_counts(s, r = 5)) %>%
#    bind_rows()
#  
#  sim_replicates %>% head
#  
#  # combine the sample and replicate datasets
#  sim_data <- sim_sample %>%
#    inner_join(sim_replicates) %>%
#    mutate(observed_rho = round(successes / n, 4))
#  
#  sim_data %>% head
#  write_csv(sim_data, path = "sim_data.csv")
#  
#  # calculate HDIs
#  stan_result <- sample_posterior(m = sim_data %>% rename(Subset = subset_qa_qs_qn_x_n),
#                                  successes_field = "successes", totals_field = "n")
#  sim_hdi <- estimate_hdi(r = stan_result) %>%
#      separate(subset, into = c("subset", "qa", "qs", "qn", "x", "n"), sep = "_") %>%
#      mutate(n = as.numeric(n)) %>%
#      mutate(obs.range = obs.max - obs.min) %>%
#      mutate(hdi.range = posterior.hdi.high - posterior.hdi.low)
#  
#  sim_hdi %>% head
#  write_csv(sim_hdi, path = "sim_hdi.csv")

## ------------------------------------------------------------------------
# load pre-computed datasets
sim_data <- read_csv(file = "sim_data.csv", col_types = cols())
sim_data %>% head
sim_hdi <- read_csv(file = "sim_hdi.csv", col_types = cols())
sim_hdi %>% head

## ----fig.width=6, fig.height=4-------------------------------------------
sim_data %>% 
ggplot() +
geom_abline(slope = 1, color = "gray70", lty = 2, lwd = 0.25) +
geom_point(aes(x = expected_rho, y = observed_rho, color = replicate_id), shape = 1, size = 1) +
ggtitle("Replicate variability in our simulated dataset")

## ----fig.width=10, fig.height=5------------------------------------------
p1 <- sim_hdi %>% 
    ggplot(aes(x = n, y = hdi.range)) +
    geom_point(alpha = 0.5) +
    ggtitle("HDI range decreases with subset size")

p2 <- sim_hdi %>% 
    ggplot(aes(x = obs.range, y = hdi.range)) +
    geom_point(alpha = 0.5) +
    ggtitle("HDI range increases with replicate variability")

gridExtra::grid.arrange(p1, p2, nrow = 1)

## ----fig.width=6, fig.height=4-------------------------------------------
i <- sample(seq(1, dim(sim_hdi)[1]), size = 10, replace = FALSE)
sel_subsets <- paste("S", i, sep = "")
dodge_width <- 0.7

sim_hdi %>% 
    filter(subset %in% sel_subsets) %>% 
    ggplot(aes(x = subset)) +
    # observed
    geom_errorbar(aes(ymin = obs.min, ymax = obs.max), 
                  alpha = 0.4, size = 5, lty = 1, position = position_dodge(width = dodge_width),
                  width = 0) +
    # estimated
    geom_errorbar(aes(ymin = posterior.hdi.low, ymax = posterior.hdi.high), 
                  size = 0.5, alpha = 1, width = 0.6, position = position_dodge(width = dodge_width)) +
    geom_point(aes(y = posterior.mean), size = 2, shape = 1, 
               position = position_dodge(width = dodge_width)) +
    ylab("METRIC.Recall") +
    xlab("") +
    ylim(0, 1) +
    coord_flip() +
    ggtitle("HDI estimates in a random selection of simulated subsets")

## ------------------------------------------------------------------------
obs_in_hdi <- sim_hdi %>% 
    mutate(obs_min_in_hdi = ifelse(posterior.hdi.low <= obs.min, TRUE, FALSE)) %>% 
    mutate(obs_max_in_hdi = ifelse(posterior.hdi.high >= obs.max, TRUE, FALSE)) %>% 
    mutate(all_obs_in_hdi = obs_min_in_hdi & obs_max_in_hdi)

obs_in_hdi %>% 
    group_by(all_obs_in_hdi) %>% 
    summarise(n = n())

