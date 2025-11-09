#' Upper and Lower Confidence Bound Acquisition Function
#' 
#' @mu vector of length m. Mean of a Gaussian process at m points.
#' @sigma vector of length m. The diagonal of the covariance matrix of a
#' Gaussian process evaluated at m points.
#' @param kappa scalar, exploration/exploitation trade off
#' @task one of "max" or "min", indicating the optimisation problem
#'
#' @return CB, vector of length m
#' 
#' 

library(ggplot2)
library(magrittr)
set.seed(4444)

objective_function <- function(x) {
  sin(12 * x) * x + 0.5 * x^2
}
X_pred <- matrix(seq(0, 1, length.out = 100), 100, 1)
y_pred <- objective_function(X_pred)

ggplot(data = tibble::tibble(x = X_pred, y = y_pred)) +
  geom_line(aes(x = x, y = y)) +
  theme_minimal() +
  labs(x = "x", y = "f(x)", title = "Objective Function")

#' RBF Kernel
#'
#' @param X1 matrix of dimensions (n, d). Vectors are coerced to (1, d).
#' @param X2 matrix of dimensions (m, d). Vectors are coerced to (1, d).
#' @param l length scale
#' @param sigma_f scale parameter 
#'
#' @return matrix of dimensions (n, m)
rbf_kernel <- function(X1, X2, l = 1.0, sigma_f = 1.0) {
  if (is.null(dim(X1))) dim(X1) <- c(1, length(X1))
  if (is.null(dim(X2))) dim(X2) <- c(1, length(X2))
  sqdist <- (- 2*(X1 %*% t(X2))) %>%
    add(rowSums(X1**2, dims = 1)) %>%
    sweep(2, rowSums(X2**2, dims = 1), `+`)
  sigma_f**2 * exp(-0.5 / l**2 * sqdist)
}

#' Random Samples from a Multivariate Gaussian
#' 
#' This implementation is similar to MASS::mvrnorm, but uses chlosky
#' decomposition instead. This should be more stable but is less efficient than
#' the MASS implementation, which recycles the eigen decomposition for the
#' sampling part.
#'
#' @param n number of samples to sample
#' @param mu the mean of each input dimension
#' @param sigma the covariance matrix
#' @param epsilon numerical tolerance added to the diagonal of the covariance
#'  matrix. This is necessary for the Cholesky decomposition, in some cases.
#'
#' @return numerical vector of n samples
rmvnorm <- function(n = 1, mu, sigma, epsilon = 1e-6) {
  p <- length(mu)
  if(!all(dim(sigma) == c(p, p))) stop("incompatible dimensions of arguments")
  ev <- eigen(sigma, symmetric = TRUE)$values
  if(!all(ev >= -epsilon*abs(ev[1L]))) {
    stop("The covariance matrix (sigma) is not positive definite")
  }
  cholesky <- chol(sigma + diag(p)*epsilon)
  sample <- rnorm(p*n, 0, 1)
  dim(sample) <- c(n, p)
  sweep(sample %*% cholesky, 2, mu, FUN = `+`)
}

#' Get Parameters of the Posterior Gaussian Process
#'
#' @param kernel kernel function used for the Gaussian process
#' @param X_pred matrix (m, d) of prediction points
#' @param X_train matrix (n, d) of training points
#' @param y_train column vector (n, d) of training observations
#' @param noise scalar of observation noise
#' @param ... named parameters for the kernel function
#'
#' @return list of mean (mu) and covariance (sigma) for the Gaussian
posterior <- function(kernel, X_pred, X_train, y_train, noise = 1e-8, ...) {
  if (is.null(dim(X_pred))) dim(X_pred) <- c(length(X_pred), 1)
  if (is.null(dim(X_train))) dim(X_train) <- c(length(X_train), 1)
  if (is.null(dim(y_train))) dim(y_train) <- c(length(y_train), 1)
  K <- kernel(X_train, X_train, ...) + noise**2 * diag(dim(X_train)[[1]])
  K_s <- kernel(X_train, X_pred, ...)
  K_ss <- kernel(X_pred, X_pred, ...) + 1e-8 * diag(dim(X_pred)[[1]])
  K_inv <- solve(K)
  mu <- (t(K_s) %*% K_inv) %*% y_train
  sigma <- K_ss - (t(K_s) %*% K_inv) %*% K_s
  list(mu = mu, sigma = sigma)
}

#' Negative log-Likelihood of a Kernel
#'
#' @param kernel kernel function
#' @param X_train matrix (n, d) of training points
#' @param y_train column vector (n, d) of training observations
#' @param noise scalar of observation noise
#'
#' @return function with kernel parameters as input and negative log likelihood
#' as output
nll <- function(kernel, X_train, y_train, noise) {
  function(params) {
    n <- dim(X_train)[[1]]
    K <- rlang::exec(kernel, X1 = X_train, X2 = X_train, !!!params)
    L <- chol(K + noise**2 * diag(n))
    a <- backsolve(r = L, x = forwardsolve(l = t(L), x = y_train))
    0.5*t(y_train)%*%a + sum(log(diag(L))) + 0.5*n*log(2*pi)
  }
}

#' Gaussian Process Regression
#'
#' @param kernel kernel function
#' @param X_train matrix (n, d) of training points
#' @param y_train column vector (n, d) of training observations
#' @param noise scalar of observation noise
#' @param ... parameters of the kernel function with initial guesses. Due to the
#' optimiser used, all parameters must be given and the order unfortunately
#' matters
#'
#' @return function that takes a matrix of prediction points as input and
#' returns the posterior predictive distribution for the output
gpr <- function(kernel, X_train, y_train, noise = 1e-8, ...) {
  kernel_nll <- nll(kernel, X_train, y_train, noise)
  param <- list(...)
  opt <- optim(par = rep(1, length(param)), fn = kernel_nll)
  opt_param <- opt$par
  function(X_pred) {
    post <- rlang::exec(
      posterior,
      kernel = kernel,
      X_pred = X_pred,
      X_train = X_train,
      y_train = y_train,
      noise = noise,
      !!!opt_param
    )
    list(
      mu = post$mu,
      sigma = diag(post$sigma),
      Sigma = post$sigma,
      parameters = set_names(opt_param, names(param))
    )
  }
}

X_train <- matrix(c(0.1, 0.2,0.4, 0.7, 0.75), 5, 1)
y_train <- objective_function(X_train)
gp <- gpr(rbf_kernel, X_train, y_train, noise = 1e-8, l = 1, sigma_f = 1)
y_min <- min(y_train)
y_max <- max(y_train)
post_pred <- gp(X_pred)
mu <- post_pred$mu
sigma <- post_pred$sigma

gp_plot <- tibble::tibble(
  mu = mu,
  uncertainty = 1.96*sqrt(sigma),
  upper = mu + uncertainty,
  lower = mu - uncertainty,
  x = X_pred,
  f = y_pred
) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = mu, colour = "Mean")) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = "89% interval"),
    alpha = 0.2
  ) +
  geom_point(
    data = tibble::tibble(x = X_train, y = y_train),
    aes(x = x, y = y, shape = "Training point"),
    colour = "#fb8500",
    size = 5
  ) +
  geom_line(mapping = aes(y = f, colour = "True function")) +
  scale_shape_manual(values = c("Training point" = "+")) +
  scale_fill_manual(values = c("89% interval" = "#219ebc")) +
  labs(shape = "") +
  theme_minimal() +
  labs(
    y = "y",
    x = "",
    colour = "",
    fill = ""
  ) +
  theme(panel.grid = element_blank(), axis.text.x = element_blank())
gp_plot

acquisition_plot <- function(X_pred,
                             acquisition_function,
                             gp_plot,
                             xt1,
                             label = "EI",
                             title = "") {
  p1 <- tibble::tibble(
    x = X_pred,
    a = acquisition_function
  ) %>%
    ggplot() +
    geom_line(aes(x = x, y = a, colour = label)) +
    geom_vline(xintercept = xt1, linetype = 2) +
    theme_minimal() +
    labs(x = "", y = label, colour = "") +
    theme(panel.grid = element_blank())
  p2 <- gp_plot +
    geom_vline(xintercept = xt1, linetype = 2) +
    labs(title = title)
  aligned_plots <- cowplot::align_plots(p2, p1 , align = "v")
  cowplot::plot_grid(aligned_plots[[1]], aligned_plots[[2]], ncol = 1)
}



confidence_bound <- function(mu, sigma, kappa, task = "max") {
  if (task == "max") return(mu + kappa * sigma)
}


ucb <- confidence_bound(mu, sigma, kappa = 2, "max")
xt1 <- X_pred[which.max(ucb)]
acquisition_plot(X_pred, ucb, gp_plot, xt1, "UCB", "Upper Confidence Bound")