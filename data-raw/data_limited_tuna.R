# South Atlantic albacore tuna catch and CPUE from Meyer and Millar (1999),
# for the surplus production model, and posterior medians from a Stan fit of
# the same model.

tuna <- data.frame(
  year = 1967:1989,
  # Catch in metric tons. Meyer and Millar (1999) give it in thousands of
  # metric tons.
  catch = 1000 * c(
    15.9, 25.7, 28.5, 23.7, 25.0, 33.3, 28.2, 19.7, 17.5, 19.3, 21.6, 23.1,
    22.5, 22.5, 23.6, 29.1, 14.4, 13.2, 28.4, 34.6, 37.5, 25.9, 25.3
  ),
  # CPUE in kg per 100 hooks
  index = c(
    61.89, 78.98, 55.59, 44.61, 56.89, 38.27, 33.84, 36.13, 41.95, 36.63,
    36.33, 38.82, 34.32, 37.64, 34.01, 32.16, 26.88, 36.61, 30.07, 30.75,
    23.36, 22.36, 21.91
  )
)

# Catch is known without error in the surplus production model, so it has no
# uncertainty.
data_limited_tuna <- rbind(
  data.frame(
    type = "catch",
    fleet = "fleet1",
    age = NA_real_,
    length = NA_real_,
    timing = tuna[["year"]],
    observed = tuna[["catch"]],
    unit = "mt",
    uncertainty = NA_character_
  ),
  data.frame(
    type = "index",
    fleet = "survey1",
    age = NA_real_,
    length = NA_real_,
    timing = tuna[["year"]],
    observed = tuna[["index"]],
    # FIMSFrame() accepts only "mt" or "number" for an index. Catchability
    # scales the index to biomass, so the unit does not change the fit.
    unit = "mt",
    uncertainty = "~ dlnorm(meanlog = log_index_expected, sdlog = 0.1)"
  )
)

usethis::use_data(data_limited_tuna, overwrite = TRUE)

# Stan fit of the Meyer and Millar (1999) Schaefer model (m = 2), as in Best
# and Punt (2020). Depletion P has lognormal process error in every year, with
# P_1 centered on 1. log q is the mean of log(I / (P K)) (Walters and Ludwig
# 1994). The priors on the variances are inverse gamma, as in Meyer and
# Millar (1999). Their prior on K is for K in thousands of metric tons, so
# log(1000) is added to its log-scale mean. This needs rstan, which is not a
# FIMS dependency.
stan_code <- "
functions {
  real pella_tomlinson(real P0, real r, real K, real m, real C) {
    return P0 + r / (m - 1) * P0 * (1 - P0 ^ (m - 1)) - C / K;
  }
}
data {
  int<lower=0> T;
  vector[T] C;
  vector[T] I;
  real m;
}
parameters {
  real<lower=0> r;
  real<lower=0> K;
  real<lower=0> sigma2;
  real<lower=0> tau2;
  vector<lower=0>[T] P;
}
transformed parameters {
  vector[T] P_med;
  vector[T] Z;
  real log_q_hat;
  P_med[1] = 1;
  for (t in 2:T) {
    P_med[t] = fmax(pella_tomlinson(P[t - 1], r, K, m, C[t - 1]), 0.001);
  }
  for (t in 1:T) {
    Z[t] = log(I[t]) - log(P[t]) - log(K);
  }
  log_q_hat = mean(Z);
}
model {
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  K ~ lognormal(5.042905 + log(1000), 1 / sqrt(3.7603664));
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);
  P ~ lognormal(log(P_med), sqrt(sigma2));
  Z ~ normal(log_q_hat, sqrt(tau2));
}
generated quantities {
  real P_final;
  P_final = lognormal_rng(
    log(fmax(pella_tomlinson(P[T], r, K, m, C[T]), 0.001)), sqrt(sigma2)
  );
}
"
stan_fit <- rstan::stan(
  model_code = stan_code,
  data = list(T = nrow(tuna), C = tuna[["catch"]], I = tuna[["index"]], m = 2),
  warmup = 5000, iter = 30000, chains = 4, seed = 1,
  control = list(adapt_delta = 0.99)
)
posterior <- as.data.frame(stan_fit)
posterior_median <- function(name) stats::median(posterior[[name]])
data_limited_tuna_results <- data.frame(
  label = c(
    "growth_rate", "carrying_capacity", "sigma2_obs", "sigma2_depletion",
    rep("depletion", nrow(tuna) + 1), "q"
  ),
  median = c(
    posterior_median("r"), posterior_median("K"), posterior_median("tau2"),
    posterior_median("sigma2"),
    vapply(paste0("P[", seq_len(nrow(tuna)), "]"), posterior_median, numeric(1)),
    posterior_median("P_final"), exp(posterior_median("log_q_hat"))
  )
)

usethis::use_data(data_limited_tuna_results, overwrite = TRUE)
