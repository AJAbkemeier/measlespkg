#' Perform one round of model fitting
#'
#' @param x An object used to select a method. Should either be `panelPomp`
#'   object or an object of class `fit_results`.
#' @param write_results_to File path to save Rds file containing results to.
#' @param ncores Number of cores to use.
#' @param np_mif2 Number of particles to use when running `mif2()`.
#' @param cooling_frac Cooling fraction to use when running `mif2()`.
#' @param rw_sd Random walk standard deviation to use when running `mif2()`.
#' @param nmif Number of iterations to use when running `mif2()`.
#' @param np_eval Number of particles to use when running `eval_logLik()`.
#' @param nreps_eval Number of replications to use when running `eval_logLik()`.
#' @param print_times Boolean for whether times to run `mif2()` and
#'   `eval_logLik()` should be printed.
#' @param block Boolean specifying whether to perform block resampling of
#'   specific parameters.
#' @param ... Additional arguments for other methods.
#'
#' @return Object of class `fit_results` containing a list of `mif2` objects and
#'   a list of `EL_list` objects.
#' @export
#'
run_round = function(
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
    ...
){
  UseMethod("run_round")
}
