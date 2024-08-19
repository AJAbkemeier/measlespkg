arma_mechanics = function(ar = 1, ma = 1, shared_params = NULL){
 # TODO make it work for ar = 0 and ma = 0
  xp = paste0("X", seq_len(ar)) |> setdiff("X")
  wp =  paste0("W", seq_len(ma)) |> setdiff("W")
  arh = paste0("ar", seq_len(ar)) |> setdiff("ar")
  mah = paste0("ma", seq_len(ma)) |> setdiff("ma")

  rproc <- pomp::Csnippet(c("
    W = rnorm(0, sigma);
    X = W",
    paste0("+ ",arh,"*",xp),
    paste0("+ ",mah,"*",wp),
    ";
    ",
    paste0(xp, " = ", c("X", utils::head(xp, ar - 1)),";
    "),
    paste0(wp, " = ", c("W", utils::head(wp, ma - 1)),";
    ")
  ))

  dmeas <- pomp::Csnippet(paste0("
    if(ISNA(Y)) {lik = 0;} else {
      lik = dnorm(Y, X, sigma, 1);
    }
    if (!give_log) lik = exp(lik);
  "))

  rmeas <- pomp::Csnippet(paste0("
    Y = X;
  "))

  rinit <- pomp::Csnippet(c("
    W = 0;
    X = 0;
  ",
  paste0(
    xp, " = 0;
    "
  ),
  paste0(
    wp, " = 0;
    "
  )
  ))

  pt <- pomp::parameter_trans(
    log = c("sigma"),
  )

  states = c(xp, wp) |> union(c("X", "W"))
  paramnames = c("sigma", arh, mah)

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
