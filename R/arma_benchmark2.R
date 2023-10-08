#' Produce log-ARMA log-likelihood benchmark for panelPomp and spatPomp models
#'
#' @param xpomp A panelPomp or spatPomp object.
#' @param order A triple (p, d, q) for the ARIMA model fitted to the data.
#'   Function code assumes that d will be set to 0.
#' @param na.rm Should NA's be removed when summing conditional log likelihoods
#'   to obtain unit log likelihoods and total log likelihoods?
#'
#' @return A list with a named vector of unit log likelihoods, a vector
#'   containing the total log likelihood, and a matrix of conditional log
#'   likelihoods.
#' @export
arma_benchmark2 = function (xpomp, order = c(2, 0, 1), na.rm = FALSE) {
  x <- obs2(xpomp)
  fit <- apply(x, 1, function(y, order){
    arima2::arima(log(y + 1), order = order)
  }, order = order)
  unit <- sapply(fit, function(z) z$loglik) -
    apply(x, 1, function(y) sum(log(y + 1), na.rm = na.rm))
  total <- sum(unit, na.rm = na.rm)
  cond <- t(sapply(fit, function(f){
    unlist(stats::dnorm(f$resid, mean = 0, sd = sqrt(f$sigma2), log = T))
  })) - log(x + 1)
  list(unit = unit, total = total, cond = cond)
}
