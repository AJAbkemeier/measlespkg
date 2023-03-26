#' Perform one round of model fitting
#'
#' @inheritParams run_round
#' @param x Object of class `fit_results`.
#' @param top_n_fits Number of best fits to continue from `x`.
#' @param combine Boolean specifying whether top fits for different units from
#' `x` should be combined when selecting best fits to continue.
#'
#' @return Object of class `fit_results` containing a list of `mif2` objects
#' and a list of `EL_list` objects.
#' @export
#'
run_round.fit_results = function(
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
    top_n_fits = length(x),
    combine = FALSE,
    ...
){
  i = NULL # prevents devtools::check() note
  starting_pparams_list = duplicate_top_pparams(
    x$EL_out[[length(x$EL_out)]],
    out_length = length(x$mif2_out),
    top_n = top_n_fits,
    combine = combine
  )
  doParallel::registerDoParallel(cores = ncores)
  doRNG::registerDoRNG()
  fit_results = pomp::bake(file = write_results_to, {
    if(print_times) start_t = Sys.time()
    mif2_out = foreach::foreach(
      i = 1:length(x$mif2_out),
      .packages = c("panelPomp", "pomp"),
      .combine = c
    ) %dopar% {
      panelPomp::mif2(
        x$mif2_out[[i]],
        Np = np_mif2,
        cooling.fraction.50 = cooling_frac,
        rw.sd = rw_sd,
        cooling.type = "geometric",
        Nmif = nmif,
        shared.start = starting_pparams_list[[i]]$shared,
        specific.start = starting_pparams_list[[i]]$specific,
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
    # for(j in seq_along(mif2_out)){
    #   mif2_out[[j]] = lapply(seq_along(mif2_out[[j]]), function(k) {
    #     mif2_old = x$mif2_out[[j]][[k]]
    #     mif2_new = mif2_out[[j]][[k]]
    #     ndone = mif2_old@Nmif
    #     mif2_old@traces[ndone + 1, "loglik"] =
    #       mif2_new@traces[1L, "loglik"]
    #     mif2_new@traces = rbind(
    #       mif2_old@traces,
    #       mif2_new@traces[-1L, colnames(mif2_old@traces)]
    #     )
    #     names(dimnames(mif2_new@traces)) = c("iteration", "variable")
    #     mif2_new@Nmif = as.integer(ndone + nmif)
    #     mif2_new
    #   }) |>
    #     panelPomp()
    # }
    new_fit_results(mif2_out = mif2_out, EL_out = c(x$EL_out, list(EL_out)))
  })
  fit_results
}
