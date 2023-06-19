#' @title ggsave for targets
#' @inheritParams ggplot2::ggsave
my_ggsave <- function(filename, plot, units = c("in", "cm",
        "mm", "px"), height = NA, width = NA, dpi = 300, ...) {
  ggsave(
    filename = paste0(filename, ".png"),
    plot = plot,
    height = height,
    width = width,
    units = units,
    dpi = dpi,
    ...
  )
  ggsave(
    filename = paste0(filename, ".pdf"),
    plot = plot,
    height = height,
    width = width,
    units = units,
    dpi = dpi,
    ...
  )
  str_c(filename, c(".png", ".pdf"))
}

#' @title write_csv for targets
#' @inheritParams readr::write_csv
my_write_csv <- function(x, path, append = FALSE, col_names = !append) {
    write_csv(x, path, append = FALSE, col_names = !append)
    paste(path)
}


#' @title Simulate data
simulate_data <- function(n = 100) {
  x <- rnorm(n)
  alpha <- 1
  beta <- -1
  mu <- alpha + beta * x
  y <- rnorm(n, mu, 1)
  tibble(x = x, y = y)
}

#' @title Modified createFolds function
createSingleFold <- function(data, k, i) {
  fold_sizes <- floor(nrow(data) / k)
  folds <- split(data, rep(1:k, each = fold_sizes, length.out = nrow(data)))
  tmp <- folds[[i]]
  list(N = nrow(tmp), x = tmp$x, y = tmp$y)

}

my_loo <- function(x) x$loo(cores = parallel::detectCores())

generated_stacked_posteriors <- function(models, weights) {
  stacked_posterior <- list()
  # Loop through parameters
  for (param in names(models[[1]])) {
    # Initialize parameter values
    stacked_posterior[[param]] <- models[[1]][[param]] * weights[1]
    # Add weighted values from remaining models
    for (model_idx in 2:length(models)) {
      stacked_posterior[[param]] <- stacked_posterior[[param]] + models[[model_idx]][[param]] * weights[model_idx]
    }
  }
  stacked_posterior
}
