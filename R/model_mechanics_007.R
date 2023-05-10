#' spatPomp model for UK measles
#'
#' @return List of objects required for `make_spatMeaslesPomp`.
#' @export
#'
model_mechanics_007 = function(U){
  rproc <- spatPomp::spatPomp_Csnippet(
    unit_statenames = c('S','E','I','C'),
    unit_covarnames = c('pop','birthrate'),
    unit_paramnames = c('alpha','iota','R0','cohort','amplitude','gamma',
                        'sigma','muD','sigmaSE','g'),
    code = "
      int BS=0, SE=1, SD=2, EI=3, ED=4, IR=5, ID=6;
      double seas, Ifrac, dw;
      double mu[7], dN[7];
      double powVec[U];
      int u,v;
      double day = (t-floor(t))*365;

      for (u = 0 ; u < U ; u++) {
        // needed for the Ensemble Kalman filter
        // or other methods making real-valued perturbations to the state
        // reulermultinom requires integer-valued double type for states
        S[u] = S[u]>0 ? floor(S[u]) : 0;
        E[u] = E[u]>0 ? floor(E[u]) : 0;
        I[u] = I[u]>0 ? floor(I[u]) : 0;
        // pre-computing this saves substantial time
        powVec[u] = pow(I[u]/pop[u], alpha[u*alpha_unit]);
      }

      for (u = 0 ; u < U ; u++) {
        seas = (day >= 7 && day <= 100) ||
        (day >= 115 && day <= 199) ||
        (day >= 252 && day <= 300) ||
        (day >= 308 && day <= 356)
          ? 1.0 + amplitude[u*amplitude_unit] * 0.2411/0.7589 : 1.0 - amplitude[u*amplitude_unit];

        // cohort effect
        if (fabs(day - 251.0) < 0.5*dt*365)
          mu[BS] = cohort[u*cohort_unit]*birthrate[u]/dt +
          (1 - cohort[u*cohort_unit])*birthrate[u];
        else
          mu[BS] = (1.0 - cohort[u*cohort_unit])*birthrate[u];

        // we follow Park and Ionides (2019) and raise pop to the alpha power
        // He et al (2010) did not do this.
        Ifrac = pow((I[u] + iota[u*iota_unit])/pop[u], alpha[u*alpha_unit]);
        for (v=0; v < U ; v++) {
           if(v != u)
            Ifrac += g[u*g_unit]*v_by_g[u][v]*(powVec[v] - powVec[u])/pop[u];
        }

        dw = rgammawn(sigmaSE[u*sigmaSE_unit],dt);
        mu[SE] = R0[u*R0_unit]*(gamma[u*gamma_unit] + muD[u*muD_unit])*seas*Ifrac*dw/dt;
        mu[SD] = muD[u*muD_unit];
        mu[EI] = sigma[u*sigma_unit];
        mu[ED] = muD[u*muD_unit];
        mu[IR] = gamma[u*gamma_unit];
        mu[ID] = muD[u*muD_unit];

        // transitions between classes
        reulermultinom(2, S[u], &mu[SE], dt, &dN[SE]);  // SE and SD transitions
        reulermultinom(2, E[u], &mu[EI], dt, &dN[EI]);  // EI and ED transitions
        reulermultinom(2, I[u], &mu[IR], dt, &dN[IR]);  // IR and ID transitions
        dN[BS] = rpois(mu[BS]*dt);

        S[u] += dN[BS] - dN[SE] - dN[SD];
        E[u] += dN[SE] - dN[EI] - dN[ED];
        I[u] += dN[EI] - dN[IR] - dN[ID];
        C[u] += dN[IR];
      }
    "
  )

  dmeas <- spatPomp::spatPomp_Csnippet(
    unit_statenames = 'C',
    unit_obsnames = 'cases',
    unit_paramnames = c('rho','psi'),
    code = "
      double m,v;
      double tol = 1e-300;
      double mytol = 1e-5;
      int u;
      lik = 0;
      for (u = 0; u < U; u++) {
        m = rho[u*rho_unit]*(C[u] + mytol);
        v = m*(1.0 - rho[u*rho_unit] + psi[u*psi_unit]*psi[u*psi_unit]*m);
        // C < 0 can happen in bootstrap methods such as bootgirf
        if (C < 0) {lik += log(tol);} else {
          if (cases[u] > tol) {
            lik += log(pnorm(cases[u] + 0.5, m, sqrt(v) + tol, 1, 0) -
              pnorm(cases[u] - 0.5, m, sqrt(v)+tol, 1, 0) + tol);
          } else {
              lik += log(pnorm(cases[u] + 0.5, m, sqrt(v) + tol, 1, 0) + tol);
          }
        }
      }
      if(!give_log) lik = (lik > log(tol)) ? exp(lik) : tol;
    "
  )

  dunit_measure <- spatPomp::spatPomp_Csnippet(
    unit_paramnames = c('rho','psi'),
    code = "
      double mytol = 1e-5;
      double m = rho[u*rho_unit]*(C+mytol);
      double v = m*(1.0 - rho[u*rho_unit] + psi[u*psi_unit]*psi[u*psi_unit]*m);
      double tol = 1e-300;
      // C < 0 can happen in bootstrap methods such as bootgirf
      if (C < 0) {lik = 0;} else {
        if (cases > tol) {
          lik = pnorm(cases + 0.5, m, sqrt(v) + tol, 1, 0) -
            pnorm(cases - 0.5, m, sqrt(v) + tol, 1, 0) + tol;
        } else {
          lik = pnorm(cases + 0.5, m, sqrt(v) + tol, 1, 0) + tol;
        }
      }
      if(give_log) lik = log(lik);
    "
  )

  rmeas <- spatPomp::spatPomp_Csnippet(
    unit_paramnames = c('rho','psi'),
    unit_statenames = 'C',
    code = "
      double *cases = &cases1;
      double m,v;
      double tol = 1.0e-300;
      int u;
      for (u = 0; u < U; u++) {
        m = rho[u*rho_unit]*(C[u] + tol);
        v = m*(1.0 - rho[u*rho_unit] + psi[u*psi_unit]*psi[u*psi_unit]*m);
        cases[u] = rnorm(m, sqrt(v) + tol);
        if (cases[u] > 0.0) {
          cases[u] = nearbyint(cases[u]);
        } else {
          cases[u] = 0.0;
        }
      }
    "
  )

  rinit <- spatPomp::spatPomp_Csnippet(
    unit_paramnames = c('S_0','E_0','I_0'),
    unit_statenames = c('S','E','I','C'),
    unit_covarnames = 'pop',
    code = "
      double m;
      int u;
      for(u = 0; u < U; u++) {
        m = (float)(pop[u]);
        S[u] = nearbyint(m*S_0[u*S_0_unit]);
        I[u] = nearbyint(m*I_0[u*I_0_unit]);
        E[u] = nearbyint(m*E_0[u*E_0_unit]);
        C[u] = 0;
      }
    "
  )

  spp_names = c("R0","sigma","gamma","alpha","iota", "rho",
                "sigmaSE","psi","cohort","amplitude",
                "S_0","E_0","I_0", "muD", "g")
  shp_names = c("muD", "g")
  ivp_names = c("S_0", "E_0", "I_0")
  paramnames = union(spp_names, shp_names)
  log_names = c("sigma","gamma","sigmaSE","psi","R0", "muD", "alpha", "iota")
  logit_names = c("cohort", "amplitude", "rho", "S_0", "E_0", "I_0")
  log_spp_names = intersect(log_names, spp_names)
  log_shp_names = intersect(log_names, shp_names)
  logit_spp_names = intersect(logit_names, spp_names)
  logit_shp_names = intersect(logit_names, shp_names)
  total_log_names = unlist(lapply(
    c(log_shp_names, log_spp_names), function(x,U) paste0(x,1:U),U
  ))
  total_logit_names = unlist(lapply(
    c(logit_shp_names, logit_spp_names), function(x,U) paste0(x,1:U),U
  ))
  total_shp_names =
    unlist(lapply(shp_names, function(x,U) paste0(x,1:U),U))
  total_spp_names =
    unlist(lapply(spp_names, function(x,U) paste0(x,1:U),U))
  pt <- pomp::parameter_trans(log = total_log_names, logit = total_logit_names)

  list(
    rproc = rproc,
    dmeas = dmeas,
    rmeas = rmeas,
    rinit = rinit,
    dunit_measure = dunit_measure,
    pt = pt,
    paramnames = paramnames,
    spp_names = spp_names,
    shp_names = shp_names,
    ivp_names = ivp_names,
    total_shp_names = total_shp_names,
    total_spp_names = total_spp_names
  )
}
