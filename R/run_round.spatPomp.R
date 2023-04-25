#' Perform one round of model fitting
#'
#' @param x `spatPomp` object.
#' @param initial_pparams_list List of initial parameters in the format of
#' `pparams()`.
#' @inheritParams run_round
#'
#' @return Object of class `fit_results` containing a list of `ibpfd_spatPomp`
#' objects and a list of `EL_list` objects.
#' @export
#'
run_round.spatPomp = function(
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
    ibpf_out = foreach::foreach(
      i = 1:length(initial_pparams_list),
      .packages = "spatPomp"
    ) %dopar% {
      pomp::coef(x) = pparams_to_spatCoef(initial_pparams_list[[i]])
      spatPomp::ibpf(
        x,
        Np = np_mif2,
        cooling.fraction.50 = cooling_frac,
        rw.sd = make_rw_sd(rw_sd),
        cooling.type = "geometric",
        Nbpf = nmif,
        sharedParNames = "g",
        unitParNames =
          setdiff(rownames(initial_pparams_list[[i]]$specific), "muD"),
        block_size = 1,
        spat_regression = 0.1
      )
    }
    if(print_times) print(Sys.time() - start_t)
    if(print_times) start_t = Sys.time()
    EL_out = eval_logLik.ibpfd_spatPomp(
      model_obj_list = ibpf_out,
      ncores = ncores,
      np_pf = np_eval,
      nreps = nreps_eval,
      seed = NULL,
      divisor = NULL
    )
    if(print_times) print(Sys.time() - start_t)
    list(ibpf_out = ibpf_out, EL_out = list(EL_out))
  })
  fit_results
}
