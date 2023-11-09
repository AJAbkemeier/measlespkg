#' Sample initial parameters from a box.
#'
#' @inheritParams sample_initial_pparams_rc
#' @inheritParams sample_initial_pparams_ul
#' @param units_ul Character vector of unit names. Only needed for upper/lower
#'   method.
#'
#' @return A list of parameter sets in the `pparams()` format.
#' @export
#'
sample_initial_pparams = function(
  sh_ul,
  sp_ul,
  units_ul,
  sh_rc,
  sp_c,
  sp_r,
  n_draws,
  buffer = 5e-8,
  pos_params = c("R0", "mu", "sigmaSE", "iota", "sigma", "psi", "alpha"),
  unit_interval_params = c("cohort", "amplitude", "S_0", "E_0", "I_0", "R_0",
                           "rho")
){
  ul_check = sum(!missing(sh_ul), !missing(sp_ul), !missing(units_ul))
  ul_ready = ul_check == 3
  ul_incomplete = all(ul_check < 3, ul_check > 0)
  rc_check = sum(!missing(sh_rc), !missing(sp_c), !missing(sp_r))
  rc_ready = rc_check == 3
  rc_incomplete = all(rc_check < 3, rc_check > 0)

  if(ul_incomplete){
    stop("Some, but not all, necessary arguments have been supplied for the upper/lower method of specifying a sampling box.")
  }
  if(rc_incomplete){
    stop("Some, but not all, necessary arguments have been supplied for the radius/center method of specifying a sampling box.")
  }

  if(!ul_ready){
    sh_ul = NULL
    sp_ul = NULL
  }
  if(!rc_ready){
    sh_rc = NULL
    sp_r = NULL
  }
  params = c(sh_ul$param, sp_ul$param, sh_rc$param, sp_r$param)
  if(length(unique(params)) < length(params)){
    stop("One or more parameters are included in both the upper/lower arguments and the radius/center arguments.")
  }

  if(ul_ready){
    ul_out = sample_initial_pparams_ul(
      sh_ul = sh_ul,
      sp_ul = sp_ul,
      units = units_ul,
      n_draws = n_draws
    )
  } else {
    ul_out = lapply(1:n_draws, function(i) list(shared = NULL, specific = NULL))
  }
  if(rc_ready){
    rc_out = sample_initial_pparams_rc(
      sh_rc = sh_rc,
      sp_r = sp_r,
      sp_c = sp_c,
      buffer = buffer,
      n_draws = n_draws,
      pos_params = pos_params,
      unit_interval_params = unit_interval_params
    )
  } else {
    rc_out = lapply(1:n_draws, function(i) list(shared = NULL, specific = NULL))
  }

  lapply(1:n_draws, function(i){
    list(
      shared = c(ul_out[[i]]$shared, rc_out[[i]]$shared),
      specific = rbind(ul_out[[i]]$specific, rc_out[[i]]$specific)
    )
  })
}
