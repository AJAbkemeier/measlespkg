toy_mechanics_001w = function(U){
  mus = paste0("mu", 1:U)
  T_mus = paste0("T_", mus)

  rproc <- pomp::Csnippet("
    X = 0;
  ")
  set_mu_str = c("
    double mu_sp;\n
    int unit_num_int = round(unit_num); \n
    double mu_mean = (",paste0(mus, collapse = " + "),")/",U,";
    double norm = sqrt(pow(",paste0(mus, collapse = " - mu_mean, 2) + pow(")," - mu_mean, 2));
    if(unit_num_int == 1){
      mu_sp = (mu1 - mu_mean)/norm;
    }
    ",
    sapply(2:U, function(i){
      paste0("
        else if(unit_num_int == ",i,"){
          mu_sp = (mu",i," - mu_mean)/norm;
        }
      ")
    })
  ) |> paste0(collapse = "")

  dmeas <- pomp::Csnippet(paste0(
    set_mu_str,
    "
    double mu_tilde = mu_sh + w*mu_sp;
    if(ISNA(cases)) {lik = 0;} else {
      lik = dnorm(cases, mu_tilde, sigma, 1);
    }
    if (!give_log) lik = exp(lik);
  "))

  rmeas <- pomp::Csnippet(paste0(
    set_mu_str,
    "
    double mu_tilde = mu_sh + w*mu_sp;
    cases = rnorm(mu_tilde, sigma);
  "))

  rinit <- pomp::Csnippet("
    X = 0;
  ")

  pt <- pomp::parameter_trans(
    toEst = pomp::Csnippet(paste0(
      sapply(1:U, function(i){
        paste0("T_mu",i," = mu",i,";\n")
      }) |> paste0(collapse = ""),
      "if(w == 0){
        T_w = -INFINITY;
      } else {
        T_w = log(w);
      }"
    )),
    fromEst = pomp::Csnippet(paste0(
      "
      double mu_mean = (",paste0(T_mus, collapse = " + "),")/",U,";
      double norm = sqrt(pow(",paste0(T_mus, collapse = " - mu_mean, 2) + pow("),", 2));
    ",
      sapply(1:U, function(i){
        paste0("
          mu",i," = (T_mu",i," - mu_mean)/norm;
        ")
      }) |> paste0(collapse = ""),
      "if(T_w == -INFINITY){
        w = 0;
      } else {
        w = exp(T_w);
      }"
    )),
    log = c("sigma")
  )

  paramnames = c("sigma", "w", "mu_sh", mus)

  list(
    rproc = rproc,
    dmeas = dmeas,
    rmeas = rmeas,
    rinit = rinit,
    pt = pt,
    paramnames = paramnames,
    pseudo_sp = mus
  )
}
