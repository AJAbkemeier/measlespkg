#' He10 POMP model for UK measles with an auxiliary weight parameter
#'
#' @param param Name of the parameter you would like to express in terms of
#' a shared parameter and a weighted unit-specific parameter.
#' @param U Number of units in the model.
#' @param shared_params Character vector of parameters to be treated as shared.
#'
#' @return List of objects required for `make_measlesPomp`.
#' @export
#'
model_mechanics_001w = function(param, U, shared_params = "mu"){
  basic_params = c("R0", "mu", "sigma", "gamma", "alpha", "iota", "rho",
                   "sigmaSE", "psi", "cohort", "amplitude", "S_0", "E_0",
                   "I_0", "R_0")
  log_params = c("sigma","gamma","sigmaSE","psi","R0", "mu", "alpha", "iota")
  logit_params = c("cohort","amplitude", "rho")
  barycentric_params = c("S_0","E_0","I_0","R_0")
  stopifnot(length(U) == 1)
  stopifnot(length(param) == 1)
  stopifnot(param %in% basic_params)
  basic_params_trunc = basic_params[basic_params != param]
  params = paste0(param, 1:U)
  T_params = paste0("T_", params)

  set_param_str = c("
    double param_sp;\n
    int unit_num_int = round(unit_num); \n
    double param_mean = (",paste0(params, collapse = " + "),")/",U,";
    double norm = sqrt(pow(",paste0(params, collapse = " - param_mean, 2) + pow("),", 2));
    if(unit_num_int == 1){
      param_sp = (",paste0(param,1)," - param_mean)/norm;
    }
    ",
    sapply(2:U, function(i){
      paste0("
      else if(unit_num_int == ",i,"){
        param_sp = (",paste0(param,i)," - param_mean)/norm;
      }
      ")
    }),
    sapply(basic_params_trunc, function(x) paste0("double _",x," = ", x,";\n")),
    "double _",param," = ",param,"_sh + w*param_sp;\n",
    if(param %in% log_params) paste0("_",param," = exp(_",param,");"),
    if(param %in% logit_params) paste0("_",param," = expit(_",param,");")
  ) |> paste0(collapse = "")

  rproc <- pomp::Csnippet(paste0(
    set_param_str,
  "
    double beta, br, seas, foi, dw, births;
    double rate[6], trans[6];

    // cohort effect
    if (fabs(t-floor(t)-251.0/365.0) < 0.5*dt)
      br = _cohort*birthrate/dt + (1-_cohort)*birthrate;
    else
      br = (1.0-_cohort)*birthrate;

    // term-time seasonality
    t = (t-floor(t))*365.25;
    if ((t>=7 && t<=100) ||
        (t>=115 && t<=199) ||
        (t>=252 && t<=300) ||
        (t>=308 && t<=356))
        seas = 1.0+_amplitude*0.2411/0.7589;
    else
        seas = 1.0-_amplitude;

    // transmission rate
    beta = _R0*seas*(1.0 - exp(-(_gamma + _mu)*dt))/dt;

    // expected force of infection
    foi = beta*pow(I+_iota,_alpha)/pop;

    // white noise (extrademographic stochasticity)
    dw = rgammawn(_sigmaSE,dt);

    rate[0] = foi*dw/dt;  // stochastic force of infection
    rate[1] = _mu;         // natural S death
    rate[2] = _sigma;      // rate of ending of latent stage
    rate[3] = _mu;         // natural E death
    rate[4] = _gamma;      // recovery
    rate[5] = _mu;         // natural I death

    // Poisson births
    births = rpois(br*dt);

    // transitions between classes
    reulermultinom(2,S,&rate[0],dt,&trans[0]);
    reulermultinom(2,E,&rate[2],dt,&trans[2]);
    reulermultinom(2,I,&rate[4],dt,&trans[4]);

    S += births   - trans[0] - trans[1];
    E += trans[0] - trans[2] - trans[3];
    I += trans[2] - trans[4] - trans[5];
    R = pop - S - E - I;
    W += (dw - dt)/_sigmaSE;  // standardized i.i.d. white noise
    C += trans[4];           // true incidence
  "))

  dmeas <- pomp::Csnippet(paste0(
    set_param_str,
  "
    double m = _rho*C;
    double v = m*(1.0 - _rho + _psi*_psi*m);
    double tol = 1.0e-18; // 1.0e-18 in He10 model; 0.0 is 'correct'
    if(ISNA(cases)) {lik = 1;} else {
        if (C < 0) {lik = 0;} else {
          if (cases > tol) {
            lik = pnorm(cases + 0.5, m, sqrt(v) + tol, 1, 0) -
              pnorm(cases - 0.5 , m, sqrt(v) + tol, 1, 0) + tol;
          } else {
            lik = pnorm(cases + 0.5, m, sqrt(v) + tol, 1, 0) + tol;
          }
        }
      }
    if (give_log) lik = log(lik);
  "))

  rmeas <- pomp::Csnippet(paste0(
    set_param_str,
  "
    double m = _rho*C;
    double v = m*(1.0-_rho+_psi*_psi*m);
    double tol = 1.0e-18; // 1.0e-18 in He10 model; 0.0 is 'correct'
    cases = rnorm(m,sqrt(v)+tol);
    if (cases > 0.0) {
      cases = nearbyint(cases);
    } else {
      cases = 0.0;
    }
  "))

  rinit <- pomp::Csnippet(paste0(
    set_param_str,
  "
    double m = pop/(_S_0 + _E_0 + _I_0 + _R_0);
    S = nearbyint(m*_S_0);
    E = nearbyint(m*_E_0);
    I = nearbyint(m*_I_0);
    R = nearbyint(m*_R_0);
    W = 0;
    C = 0;
  "))

  pt <- pomp::parameter_trans(
    toEst = pomp::Csnippet(paste0(
      sapply(1:U, function(i){
        paste0("T_",param,i," = ",param,i,";\n")
      }) |> paste0(collapse = ""),
      "if(w == 0){
        T_w = -INFINITY;
      } else {
        T_w = log(w);
      }"
    )),
    fromEst = pomp::Csnippet(paste0(
      "
      double param_mean = (",paste0(T_params, collapse = " + "),")/",U,";
      double norm = sqrt(pow(",paste0(T_params, collapse = " - param_mean, 2) + pow("),", 2));
    ",
      sapply(1:U, function(i){
        paste0("
          ",param,i," = (T_",param,i," - param_mean)/norm;
        ")
      }) |> paste0(collapse = ""),
      "if(T_w == -INFINITY){
        w = 0;
      } else {
        w = exp(T_w);
      }"
    )),
    log = log_params[log_params != param],
    logit = logit_params[logit_params != param],
    barycentric = barycentric_params[barycentric_params != param]
  )

  paramnames = c("R0","mu","sigma","gamma","alpha","iota", "rho",
                 "sigmaSE","psi","cohort","amplitude",
                 "S_0","E_0","I_0","R_0", paste0(param,"_sh"), "w", params)
  paramnames = setdiff(paramnames, param)

  if(!all(shared_params %in% paramnames)){
    stop(
      "At least one parameter name given to shared_params is not in the model.",
      call. = FALSE
    )
  }
  if(length(intersect(param, shared_params)) != 0){
    stop(
      "There is overlap between shared_params and param.",
      call. = FALSE
    )
  }
  full_shared_params = union(shared_params, c("w", paste0(param,"_sh"), params))
  out = panel_mechanics(
    rproc = rproc,
    dmeas = dmeas,
    rmeas = rmeas,
    rinit = rinit,
    pt = pt,
    shared_params = full_shared_params,
    specific_params = setdiff(paramnames, full_shared_params)
  )
  out$pseudo_sp = params
  out
}
