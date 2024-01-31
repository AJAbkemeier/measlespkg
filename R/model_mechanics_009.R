#' panelPOMP model with log-log relationship for between iota and
#' the standardized 1950 population, as well as between psi and the
#' standardized 1950 population
#'
#' @name model_mechanics_009
#' @param shared_params Character vector of parameters to be treated as shared.
#'
#' @return List of objects required for `make_measlesPomp()`.
#' @export
#'
model_mechanics_009 = function(shared_params = "mu"){
  rproc <- pomp::Csnippet("
    double beta, br, seas, foi, dw, births;
    double rate[6], trans[6];

    // Population-varying parameters
    double gamma = exp(iota_2*std_log_pop_1950 + iota_1);

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

  dmeas <- pomp::Csnippet("
    double psi = exp(psi_2*std_log_pop_1950 + psi_1);
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

  rmeas <- pomp::Csnippet("
    double psi = exp(psi_2*std_log_pop_1950 + psi_1);
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

  rinit <- pomp::Csnippet("
    double m = pop/(S_0+E_0+I_0+R_0);
    S = nearbyint(m*S_0);
    E = nearbyint(m*E_0);
    I = nearbyint(m*I_0);
    R = nearbyint(m*R_0);
    W = 0;
    C = 0;
  ")

  pt <- pomp::parameter_trans(
    log = c("sigmaSE","R0", "mu", "alpha", "iota", "sigma"),
    logit = c("cohort","amplitude", "rho"),
    barycentric = c("S_0","E_0","I_0","R_0")
  )

  paramnames = c("R0","mu","alpha", "rho","sigmaSE","cohort","amplitude",
                 "S_0","E_0","I_0","R_0", "iota_2", "iota_1", "gamma",
                 "psi_2", "psi_1", "sigma")
  full_shared_params = union(
    shared_params, c("iota_2", "iota_1", "psi_2", "psi_1")
  )
  states = c("S", "E", "I", "R", "W", "C")

  if(!all(shared_params %in% paramnames)){
    stop(
      "At least one parameter name given to shared_params is not in the model.",
      call. = FALSE
    )
  }

  panel_mechanics(
    rproc = rproc,
    dmeas = dmeas,
    rmeas = rmeas,
    rinit = rinit,
    pt = pt,
    shared_params = full_shared_params,
    specific_params = setdiff(paramnames, full_shared_params),
    states = states
  )
}
