#' panelPOMP model for UK measles with linear model parameterization for
#' gamma and iota.
#'
#' @param use_nbinom
#' @param shared_str
#' @param reduced_model
#' @param sim_model
#' @param AK_interp
#'
#' @return
#' @export
#'
#' @examples
measles_ppomp_lmp_gi = function(use_nbinom = FALSE, shared_str = "mu",
reduced_model = FALSE, sim_model = NULL, AK_interp = FALSE){
  ## ----rproc-------------------------------------------------
  rproc <- Csnippet("
    double beta, br, seas, foi, dw, births;
    double rate[6], trans[6];

    // Population-varying parameters
    double gamma = exp(gamma1*std_log_pop_1950 + gamma0);
    double iota = exp(iota1*std_log_pop_1950 + iota0);

    // cohort effect
    if (fabs(t-floor(t)-251.0/365.0) < 0.5*dt)
      br = cohort*birthrate/dt + (1-cohort)*birthrate;
    else
      br = (1.0-cohort)*birthrate;

    // term-time seasonality
    t = (t-floor(t))*365.25;
    if ((t>=7 && t<=100) ||
        (t>=115 && t<=199) ||
        (t>=252 && t<=300) ||
        (t>=308 && t<=356))
        seas = 1.0+amplitude*0.2411/0.7589;
    else
        seas = 1.0-amplitude;

    // transmission rate
    beta = R0*seas*(1.0-exp(-(gamma+mu)*dt))/dt;

    // expected force of infection
    foi = beta*pow(I+iota,alpha)/pop;

    // white noise (extrademographic stochasticity)
    dw = rgammawn(sigmaSE,dt);

    rate[0] = foi*dw/dt;  // stochastic force of infection
    rate[1] = mu;         // natural S death
    rate[2] = sigma;      // rate of ending of latent stage
    rate[3] = mu;         // natural E death
    rate[4] = gamma;      // recovery
    rate[5] = mu;         // natural I death

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
    W += (dw - dt)/sigmaSE;  // standardized i.i.d. white noise
    C += trans[4];           // true incidence
  ")

  rproc_reduced <- Csnippet("
    double beta, seas, foi, dw, births;
    double rate[6], trans[6];

    double mu = 0.02;

    // Population-varying parameters
    double gamma = exp(gamma1*std_log_pop_1950 + gamma0);
    double iota = exp(iota1*std_log_pop_1950 + iota0);

    // term-time seasonality
    t = (t-floor(t))*365.25;
    if ((t>=7 && t<=100) ||
        (t>=115 && t<=199) ||
        (t>=252 && t<=300) ||
        (t>=308 && t<=356))
        seas = 1.0+amplitude*0.2411/0.7589;
    else
        seas = 1.0-amplitude;

    // transmission rate
    beta = R0*seas*(1.0-exp(-(gamma+mu)*dt))/dt;

    // expected force of infection
    foi = beta*(I+iota)/pop;

    // white noise (extrademographic stochasticity)
    dw = rgammawn(sigmaSE,dt);

    rate[0] = foi*dw/dt;  // stochastic force of infection
    rate[1] = mu;         // natural S death
    rate[2] = sigma;      // rate of ending of latent stage
    rate[3] = mu;         // natural E death
    rate[4] = gamma;      // recovery
    rate[5] = mu;         // natural I death

    // Poisson births
    births = rpois(birthrate*dt);

    // transitions between classes
    reulermultinom(2,S,&rate[0],dt,&trans[0]);
    reulermultinom(2,E,&rate[2],dt,&trans[2]);
    reulermultinom(2,I,&rate[4],dt,&trans[4]);

    S += births   - trans[0] - trans[1];
    E += trans[0] - trans[2] - trans[3];
    I += trans[2] - trans[4] - trans[5];
    R = pop - S - E - I;
    W += (dw - dt)/sigmaSE;  // standardized i.i.d. white noise
    C += trans[4];           // true incidence
  ")

  if(reduced_model){
    rproc = rproc_reduced
  }

  ## ----dmeasure-------------------------------------------------
  if(use_nbinom == FALSE){
    dmeas <- Csnippet("
      double m = rho*C;
      double v = m*(1.0-rho+psi*psi*m);
      double tol = 1.0e-18; // 1.0e-18 in He10 model; 0.0 is 'correct'
      if(ISNA(cases)) {lik=1;} else {
          if (C < 0) {lik = 0;} else {
            if (cases > tol) {
              lik = pnorm(cases+0.5,m,sqrt(v)+tol,1,0)-
                pnorm(cases-0.5,m,sqrt(v)+tol,1,0)+tol;
            } else {
              lik = pnorm(cases+0.5,m,sqrt(v)+tol,1,0)+tol;
            }
          }
        }
      if (give_log) lik = log(lik);
    ")
  }else{
    dmeas <- Csnippet("
      // Note that this psi is different from the normal psi.
      double m = rho*C;
      if(ISNA(cases)) {lik=1;} else {
          if (C == 0) {lik = 1;} else {
            lik = dnbinom_mu(cases, C/(psi*psi+2*psi), m, 0);
          }
        }
      if (give_log) lik = log(lik);
    ")
  }

  ## ----rmeasure-------------------------------------------------
  if(use_nbinom == FALSE){
    rmeas <- Csnippet("
      double m = rho*C;
      double v = m*(1.0-rho+psi*psi*m);
      double tol = 1.0e-18; // 1.0e-18 in He10 model; 0.0 is 'correct'
      cases = rnorm(m,sqrt(v)+tol);
      if (cases > 0.0) {
        cases = nearbyint(cases);
      } else {
        cases = 0.0;
      }
    ")
  }else{
    rmeas_nb <- Csnippet("
      // Note that this psi is different from the normal psi.
      double m = rho*C;
      cases = rnbinom_mu(C/(psi*psi+2*psi), m);
    ")
  }
  ## ----starting-params-------------------------------------------------
  # These values obtained by fitting log(param He mle)~log(1950 pop) linear models
  extra_param_start = tribble(
    ~param, ~value,
    "gamma1", -0.63695,
    "gamma0", 4.61215,
    "iota1", 1.4172,
    "iota0", -1.9611
  )

  ## ----transforms-----------------------------------------------
  pt <- parameter_trans(
    log=c("sigmaSE","R0", "mu", "alpha", "psi", "sigma"),
    logit=c("cohort","amplitude", "rho"),
    barycentric=c("S_0","E_0","I_0","R_0")
  )
  if(reduced_model){
    pt <- parameter_trans(
      log=c("sigmaSE","R0", "psi", "sigma"),
      logit=c("amplitude", "rho"),
      barycentric=c("S_0","E_0","I_0","R_0")
    )
  }

  ## ----param-names-----------------------------------------------
  paramnames = c("R0","mu","alpha", "rho","sigmaSE","cohort","amplitude",
                 "S_0","E_0","I_0","R_0", "gamma1", "gamma0", "psi",
                 "iota1", "iota0", "sigma")
  if(reduced_model){
    paramnames = c("R0", "rho","sigmaSE","amplitude",
                   "S_0","E_0","I_0","R_0", "gamma1", "gamma0", "psi",
                   "iota1", "iota0", "sigma")
  }

  ## ----everything-else-------------------------------------------------
  measles_ppomp_skel(
    rproc = rproc,
    dmeas = dmeas,
    rmeas = rmeas,
    extra_param_start = extra_param_start,
    pt = pt,
    paramnames = paramnames,
    shared_str = shared_str,
    sim_model = sim_model,
    AK_interp = AK_interp
  )
}
