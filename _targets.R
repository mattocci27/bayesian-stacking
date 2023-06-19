library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
library(cmdstanr)
library(furrr)
library(bayesplot)

source("R/functions.R")

# parallel computing on local or on the same node
plan(multicore)
options(clustermq.scheduler = "multicore")

tar_option_set(packages = c(
  "tidyverse",
  "bayesplot",
  "ggrepel",
  "patchwork",
  "janitor",
  "loo",
  "caret",
  "rprojroot"
))

# keep memory usage down to a minimum
tar_option_set(
  # garbage_collection = TRUE,
  # memory = "transient"
)

# check if it's inside a container
if (file.exists("/.dockerenv") | file.exists("/.singularity.d/startscript")) {
  Sys.setenv(CMDSTAN = "/opt/cmdstan/cmdstan-2.32.0")
  set_cmdstan_path("/opt/cmdstan/cmdstan-2.32.0")
}

cmdstan_version()

set.seed(123)

k <- 4

mapped <- tar_map(
  values = list(i = 1:k),
  tar_target(
    folds,
    createSingleFold(simulated_data, k, i)
  ),
  tar_stan_mcmc(
    fit_folds,
    "stan/model.stan",
    data = folds,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 1000,
    iter_sampling = 1000,
    adapt_delta = 0.95,
    max_treedepth = 15,
    seed = 123,
    return_draws = TRUE,
    return_diagnostics = TRUE,
    return_summary = TRUE
    )
)

# Create a new tar_map for loo calculations
loo_mapped <- tar_map(
  values = list(mcmc = rlang::syms(paste0("fit_folds_mcmc_model_", 1:k))),
  tar_target(
    loo,
    my_loo(mcmc)
    )
)

combined <- tar_combine(
  combined_loo,
  loo_mapped[["loo"]],
  command = list(!!!.x)
)

combined2 <- tar_combine(
  combined_draws,
  mapped[["fit_folds_draws_model"]],
  command = list(!!!.x)
)

list(
  tar_target(
    simulated_data,
    simulate_data(n = 1000)
  ),
  tar_stan_mcmc(
    fit_full,
    "stan/model.stan",
    data = list(N = nrow(simulated_data), y = simulated_data$y, x = simulated_data$x),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 1000,
    iter_sampling = 1000,
    adapt_delta = 0.95,
    max_treedepth = 15,
    seed = 123,
    return_draws = TRUE,
    return_diagnostics = TRUE,
    return_summary = TRUE
    ),
  mapped,
  loo_mapped,
  combined,
  combined2,
  tar_target(
    model_weights,
    loo_model_weights(combined_loo, method = "stacking")
  ),
  tar_target(
    stacked_posterior,
    generated_stacked_posteriors(combined_draws, model_weights)
  ),
  NULL
)
