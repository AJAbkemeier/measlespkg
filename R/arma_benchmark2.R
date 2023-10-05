#' Produce log-ARMA log-likelihood benchmark for panelPomp and spatPomp models
#'
#' @param xpomp A panelPomp or spatPomp object.
#' @param order A triple (p, d, q) for the ARIMA model fitted to the data.
#'   Function code assumes that d will be set to 0.
#'
#' @return
#' @export
#'
#' @examples
arma_benchmark2 = function (xpomp, order = c(2, 0, 1)) {
  x <- obs2(xpomp)
  fit <- apply(x, 1, function(y, order){
    stats::arima(log(y + 1), order = order)
  }, order = order)
  unit <- sapply(fit, function(z) z$loglik) -
    apply(x, 1, function(y) sum(log(y + 1)))
  total <- sum(unit)
  cond <- t(sapply(fit, function(f){
    unlist(stats::dnorm(f$resid, mean = 0, sd = sqrt(f$sigma2), log = T))
  })) - log(x + 1)
  list(unit = unit, total = total, cond = cond)
}
