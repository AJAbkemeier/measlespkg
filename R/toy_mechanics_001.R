toy_mechanics_001 = function(shared_params = NULL){

  rproc <- pomp::Csnippet("
    X = 0;
  ")

  dmeas <- pomp::Csnippet(paste0("
    if(ISNA(Y)) {lik = 0;} else {
      lik = dnorm(Y, mu, sigma, 1);
    }
    if (!give_log) lik = exp(lik);
  "))

  rmeas <- pomp::Csnippet(paste0("
    Y = rnorm(mu, sigma);
  "))

  rinit <- pomp::Csnippet("
    X = 0;
  ")

  pt <- pomp::parameter_trans(
    log = c("sigma"),
  )

  states = "X"
  paramnames = c("sigma", "mu")

  list(
    rproc = rproc,
    dmeas = dmeas,
    rmeas = rmeas,
    rinit = rinit,
    pt = pt,
    paramnames = paramnames,
    shared_params = shared_params,
    specific_params = setdiff(paramnames, shared_params),
    states = states
  )
}
