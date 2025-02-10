#' Produce an AR(1) negative binomial benchmark for panelPomp models
#'
#' @param x A panelPomp object or a matrix of data where each row is a unit and
#'   each column is a time.
#' @param init Initial value \eqn{Y_0}. If `NULL`, uses \eqn{Y_0 = Y_1}.
#' @param start_vals Starting values for the parameter estimates.
#' @param transform Boolean, whether or not to use variable transformations in
#'   the optimization.
#' @param a Only used if `transform == TRUE`. `a` is a numeric representing the
#'   maximum value for the AR1 coefficient, and `-a` is the minimum value.
#' @param ... other parameters to be passed into the `optim` function
#'
#' @return A list with a named vector of estimate parameters, unit log
#'   likelihoods, a vector containing the total log likelihood, and a matrix of
#'   conditional log likelihoods.
#' @export
#'
ar1NB_benchmark <- function(
  x,
  init = NULL,
  start_vals = c(10, 0.5, 1),
  transform = TRUE,
  a = 2,
  ...
) {
  UseMethod("ar1NB_benchmark")
}

#' @exportS3Method measlespkg::ar1NB_benchmark
ar1NB_benchmark.panelPomp <- function(
    x,
    init = NULL,
    start_vals = c(10, 0.5, 1),
    transform = TRUE,
    a = 2,
    ...
) {
  x <- obs2(x)
  ar1NB_benchmark.matrix(
    x, init = init, start_vals = start_vals, transform = transform, a = 2, ...
  )
}

#' @exportS3Method measlespkg::ar1NB_benchmark
ar1NB_benchmark.matrix <- function(
    x,
    init = NULL,
    start_vals = c(10, 0.5, 1),
    transform = TRUE,
    a = 2,
    ...
) {
  fit <- apply(x, 1, function(y){
    y0 = if(is.null(init)) y[[1]] else init
    haitipkg::ar1_NegBinom(
      y, init = init, start_vals = start_vals, transform = transform, a = 2, ...
    )
  })
  unit <- sapply(fit, function(z) z$ll)
  total <- sum(unit, na.rm = TRUE)
  cond <- t(sapply(fit, function(f){f$cond_ll}))
  thetas <- t(sapply(fit, function(f){f$theta}))
  list(thetas = thetas, unit = unit, total = total, cond = cond)
}

