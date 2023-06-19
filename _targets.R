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
  "caret"
))

# keep memory usage down to a minimum
tar_option_set(
  # garbage_collection = TRUE,
  # memory = "transient"
)

# check if it's inside a container
if (file.exists("/.dockerenv") | file.exists("/.singularity.d/startscript")) {
  Sys.setenv(CMDSTAN = "/opt/cmdstan/cmdstan-2.29.2")
  set_cmdstan_path("/opt/cmdstan/cmdstan-2.29.2")
}

cmdstan_version()

set.seed(123)

# values <- tibble(n = c(110, 90))

k <- 4
mapped <- tar_map(
  values = list(i = 1:k),
  tar_target(
    folds,
    createSingleFold(simulated_data, k, i)
  ),
  tar_stan_mcmc(
      fit,
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
  values = list(mcmc = rlang::syms(paste0("fit_mcmc_linear_", 1:k))),
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

list(
  tar_target(
    simulated_data,
    simulate_data(n = 1000)
  ),
  mapped,
  loo_mapped,
  combined,
  tar_target(
    model_weights,
    loo_model_weights(combined_loo, method = "stacking")
  ),
  NULL
)
