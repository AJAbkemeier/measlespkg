#' Perform one round of model fitting
#'
#' @param x `panelPomp` object.
#' @param initial_pparams_list List of initial parameters in the format of
#'   `pparams()`.
#' @inheritParams run_round
#'
#' @return Object of class `fit_results` containing a list of `mif2` objects and
#'   a list of `EL_list` objects.
#' @export
#'
run_round.panelPomp = function(
    x,
    write_results_to,
    ncores,
    np_mif2,
    cooling_frac,
    rw_sd,
    nmif,
    block = FALSE,
    np_eval,
    nreps_eval,
    print_times = FALSE,
    initial_pparams_list,
    ...
){
  i = NULL # prevents check() note
  doParallel::registerDoParallel(cores = ncores)
  doRNG::registerDoRNG()
  fit_results = pomp::bake(file = write_results_to, {
    if(print_times) start_t = Sys.time()
    mif2_out = foreach::foreach(
      i = 1:length(initial_pparams_list),
      .packages = "panelPomp",
      .combine = c
    ) %dopar% {
      panelPomp::mif2(
        x,
        Np = np_mif2,
        cooling.fraction.50 = cooling_frac,
        rw.sd = rw_sd,
        cooling.type = "geometric",
        Nmif = nmif,
        shared.start = initial_pparams_list[[i]]$shared,
        specific.start = initial_pparams_list[[i]]$specific,
        block = block
      )
    }
    if(print_times) print(Sys.time() - start_t)
    if(print_times) start_t = Sys.time()
    EL_out = eval_logLik(
      model_obj_list = mif2_out,
      ncores = ncores,
      np_pf = np_eval,
      nreps = nreps_eval,
      seed = NULL,
      divisor = NULL
    )
    if(print_times) print(Sys.time() - start_t)
    new_fit_results(mif2_out = mif2_out, EL_out = list(EL_out))
  })
  fit_results
}
