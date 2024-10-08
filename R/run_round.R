#' Perform one round of model fitting
#'
#' Depending on the model used, `run_round` will use either [panelPomp::mif2] or
#' [spatPomp::ibpf] to fit it, then use `eval_logLik()` to estimate the log
#' likelihood of the fit. The results are saved using [pomp::bake].
#'
#' @param x A `panelPomp` or `spatPomp` object.
#' @param initial_pparams_list List of initial parameters in the format of
#'   `panelPomp::coef()` with `format = "list`. Each entry in the list specifies
#'   the initial parameters for one replication of the fitting algorithm.
#' @param write_results_to File path to save Rds file containing results to.
#' @param ncores Number of cores to use.
#' @param np_fitr Number of particles to use when running the fitting algorithm.
#' @param cooling_frac Cooling fraction to use when running fitting algorithm.
#' @param rw_sd_obj Object of class `rw_sd` specifying random walk standard
#'   deviations to use when running the fitting algorithm.
#' @param N_fitr Number of iterations to use when running the fitting algorithm.
#' @param np_eval Number of particles to use when running `eval_logLik()`.
#' @param nreps_eval Number of replications to use when running `eval_logLik()`.
#' @param print_times Boolean for whether times to run the fitting algorithm and
#'   `eval_logLik()` should be printed.
#' @param panel_block Boolean specifying whether to perform block resampling of
#'   specific parameters for a `panelPomp` model. Only used when `x` is a
#'   `panelPomp` object.
#' @param spat_block_size Block size used when fitting and evaluating `spatPomp`
#'   models. Only used when `x` is a `spatPomp` object.
#' @param spat_sharedParNames Character vector of parameters to be treated as
#'   shared by `ibpf()`. Only used when `x` is a `spatPomp` object.
#' @param spat_unitParNames Character vector of parameters to be treated as
#'   unit-specific by `ibpf()`. Only used when `x` is a `spatPomp` object.
#' @param spat_regression Numeric specifying the mean-regressing coefficient to
#'   use when fitting shared parameters in a `spatPomp` model. Only used when
#'   `x` is a `spatPomp` object.
#' @param rparam If using a reparameterization for GMCAP, specify which
#'   parameter is reparameterized using this argument so the reparameterization
#'   constraint can be applied before evaluating the log likelihood. Set to NULL
#'   otherwise.
#'
#' @return Object of class `fit_results` containing a list of `mif2d.ppomp` or
#'   `ibpfd_spatPomp` objects and a list of `EL_list` objects.
#' @export
#'
run_round = function(
    x,
    initial_pparams_list,
    write_results_to = NULL,
    ncores,
    np_fitr,
    cooling_frac,
    rw_sd_obj,
    N_fitr,
    panel_block = FALSE,
    np_eval,
    nreps_eval,
    print_times = FALSE,
    spat_block_size = 1,
    spat_sharedParNames = NULL,
    spat_unitParNames = NULL,
    spat_regression = 0.2,
    rparam = NULL
){
  doParallel::registerDoParallel(cores = ncores)
  RNGkind("L'Ecuyer-CMRG")
  doRNG::registerDoRNG()
  fit_results = if(is.null(write_results_to)){
    rrh(x = x, initial_pparams_list = initial_pparams_list, ncores = ncores,
        np_fitr = np_fitr, cooling_frac = cooling_frac, rw_sd_obj = rw_sd_obj,
        N_fitr = N_fitr, panel_block = panel_block, np_eval = np_eval,
        nreps_eval = nreps_eval, print_times = print_times,
        spat_block_size = spat_block_size,
        spat_sharedParNames = spat_sharedParNames,
        spat_unitParNames= spat_unitParNames, spat_regression = spat_regression,
        rparam = rparam)
  } else {
    pomp::bake(file = write_results_to, {
      rrh(x = x, initial_pparams_list = initial_pparams_list, ncores = ncores,
          np_fitr = np_fitr, cooling_frac = cooling_frac, rw_sd_obj = rw_sd_obj,
          N_fitr = N_fitr, panel_block = panel_block, np_eval = np_eval,
          nreps_eval = nreps_eval, print_times = print_times,
          spat_block_size = spat_block_size,
          spat_sharedParNames = spat_sharedParNames,
          spat_unitParNames= spat_unitParNames,
          spat_regression = spat_regression,
          rparam = rparam)
    })
  }
  fit_results
}

rrh = function(
    x,
    initial_pparams_list,
    ncores,
    np_fitr,
    cooling_frac,
    rw_sd_obj,
    N_fitr,
    panel_block,
    np_eval,
    nreps_eval,
    print_times,
    spat_block_size,
    spat_sharedParNames,
    spat_unitParNames,
    spat_regression,
    rparam
){
  if(print_times) start_t = Sys.time()
  fitr_out = run_round_helper(
    x = x,
    initial_pparams_list = initial_pparams_list,
    np_fitr = np_fitr,
    cooling_frac = cooling_frac,
    rw_sd_obj = rw_sd_obj,
    N_fitr = N_fitr,
    panel_block = panel_block,
    spat_block_size = spat_block_size,
    spat_sharedParNames = spat_sharedParNames,
    spat_unitParNames = spat_unitParNames,
    spat_regression = spat_regression
  )
  if(print_times) print(Sys.time() - start_t)
  if(!is.null(rparam)){
    for(i in seq_along(fitr_out)){
      pp = panelPomp::coef(fitr_out[[i]], format = "list")
      rparams = paste0(rparam,seq_along(fitr_out[[i]]))
      param_avg = mean(pp$shared[rparams])
      param_norm = sqrt(sum((pp$shared[rparams] - param_avg)^2))
      fitr_out[[i]]@shared[rparams] =
        (pp$shared[rparams] - param_avg)/param_norm
    }
  }
  if(print_times) start_t = Sys.time()
  EL_out = eval_logLik(
    model_obj_list = fitr_out,
    ncores = ncores,
    np_pf = np_eval,
    nreps = nreps_eval,
    seed = NULL,
    block_size = spat_block_size
  )
  if(print_times) print(Sys.time() - start_t)
  new_fit_results(fitr_out = fitr_out, EL_out = EL_out)
}

run_round_helper = function(
    x,
    initial_pparams_list,
    np_fitr,
    cooling_frac,
    rw_sd_obj,
    N_fitr,
    ...
){
  UseMethod("run_round_helper")
}

#' @exportS3Method measlespkg::run_round_helper
run_round_helper.panelPomp = function(
    x,
    initial_pparams_list,
    np_fitr,
    cooling_frac,
    rw_sd_obj,
    N_fitr,
    panel_block,
    ...
){
  z = NULL # prevents check() note
  mif2_out = foreach::foreach(
    z = initial_pparams_list,
    .packages = "panelPomp"
  ) %dopar% {
    use_shared = !is.null(z$shared)
    use_specific = !is.null(z$specific)
    args = c(
      list(
        x,
        Np = np_fitr,
        cooling.fraction.50 = cooling_frac,
        rw.sd = rw_sd_obj,
        cooling.type = "geometric",
        Nmif = N_fitr,
        block = panel_block
      ),
      list(shared.start = z$shared)[use_shared],
      list(specific.start = z$specific)[use_specific]
    )
    do.call(panelPomp::mif2, args = args)
  }
  mif2_out
}

#' @exportS3Method measlespkg::run_round_helper
run_round_helper.spatPomp = function(
    x,
    initial_pparams_list,
    np_fitr,
    cooling_frac,
    rw_sd_obj,
    N_fitr,
    spat_sharedParNames,
    spat_unitParNames,
    spat_block_size,
    spat_regression,
    ...
){
  z = NULL # prevents check() note
  ibpf_out = foreach::foreach(
    z = initial_pparams_list,
    .packages = "spatPomp"
  ) %dopar% {
    pomp::coef(x) = pparams_to_spatCoef(z)
    spatPomp::ibpf(
      x,
      Np = np_fitr,
      cooling.fraction.50 = cooling_frac,
      rw.sd = rw_sd_obj,
      cooling.type = "geometric",
      Nbpf = N_fitr,
      sharedParNames = spat_sharedParNames,
      unitParNames = spat_unitParNames,
      block_size = spat_block_size,
      spat_regression = spat_regression
    )
  }
  ibpf_out
}

