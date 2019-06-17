library("rstan")
library("HDInterval")

sample_posterior <- function(m, successes_field, totals_field,
                             b_alpha_prior = 1, b_beta_prior = 1,
                             n_mean_prior = 1000, n_variance_prior = 10000, iter = 2000) {

    # number of positives / numerator ; matrix of subsets x replicates
    p <- m %>%
        reshape2::acast(formula = Subset ~ replicate_id, value.var = successes_field)

    # total / denominator ; matrix of subsets x replicates
    q <- m %>%
        reshape2::acast(formula = Subset ~ replicate_id, value.var = totals_field)

    # number of subsets
    M <- dim(p)[1]

    # number of replicates
    N <- dim(p)[2]

    # combine
    stan_data <- list(
        p = p,
        q = q,
        M = M,
        N = N,
        b_alpha_prior = b_alpha_prior,
        b_beta_prior = b_beta_prior,
        n_mean_prior = n_mean_prior,
        n_variance_prior = n_variance_prior)

    # call stan
    mdl <- stan_model("negative-binomial.stan")
    opt_result <- optimizing(object = mdl, data = stan_data)
    s_result <- sampling(object = mdl, data = stan_data, iter = iter)

    r <- list(stan_data = stan_data, stan_fit = s_result)

    r

}

estimate_hdi <- function(r, credMass = 0.95) {

    stan_data <- r$stan_data
    s_result <- r$stan_fit
    prior.means <- rowMeans(stan_data$p/stan_data$q)
    subsets <- row.names(stan_data$p)
    result <- lapply(seq_along(subsets), function(i) {
        props = extract(s_result)$rho[, i]
        h = hdi(props, credMass = credMass)
        r = data.frame(
            i = i,
            subset = subsets[i],
            n.obs = stan_data$N,
            prior.mean = prior.means[i],
            obs.min = min(stan_data$p[i,]/stan_data$q[i,]),
            obs.max = max(stan_data$p[i,]/stan_data$q[i,]),
            posterior.mean = mean(props),
            posterior.hdi.low = h[1],
            posterior.hdi.high = h[2],
            # posterior.sample = I(list(props)),
            stringsAsFactors = FALSE
        )
        r
    }) %>% bind_rows()

    result

}