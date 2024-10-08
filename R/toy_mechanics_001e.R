toy_mechanics_001e = function(U){
  mus = paste0("mu", 1:U)
  T_mus = paste0("T_", mus)

  rproc <- pomp::Csnippet("
    X = 0;
  ")
  set_mu_str = c("
    double mu_sp;\n
    int unit_num_int = round(unit_num); \n
    if(unit_num_int == 1){
      mu_sp = mu1;
    }
    ",
    sapply(2:U, function(i){
      paste0("
        else if(unit_num_int == ",i,"){
          mu_sp = mu",i,";
        }
      ")
    })
  ) |> paste0(collapse = "")

  dmeas <- pomp::Csnippet(paste0(
    set_mu_str,
    "
    double mu_tilde = mu_sh + w*mu_sp;
    if(ISNA(Y)) {lik = 0;} else {
      lik = dnorm(Y, mu_tilde, sigma, 1);
    }
    if (!give_log) lik = exp(lik);
  "))

  rmeas <- pomp::Csnippet(paste0(
    set_mu_str,
    "
    double mu_tilde = mu_sh + w*mu_sp;
    Y = rnorm(mu_tilde, sigma);
  "))

  rinit <- pomp::Csnippet("
    X = 0;
  ")

  pt <- pomp::parameter_trans(
    log = c("sigma")
  )

  states = "X"

  paramnames = c("sigma", "w", "mu_sh", mus)

  full_shared_params = c("w", "mu_sh", mus)

  list(
    rproc = rproc,
    dmeas = dmeas,
    rmeas = rmeas,
    rinit = rinit,
    pt = pt,
    paramnames = paramnames,
    shared_params = full_shared_params,
    specific_params = setdiff(paramnames, full_shared_params),
    states = states,
    pseudo_sp = mus
  )
}
