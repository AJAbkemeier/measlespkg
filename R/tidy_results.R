#' Convert `fit_results` or `EL_list` output into tidy data frame
#'
#' @param x Object of type `fit_results` or `EL_list`.
#' @param path Character specifying where `x` was obtained from. Mostly exists
#'   for use by [measlespkg::gather_results()]. Very optional.
#'
#' @return `data.frame` where each row contains information for a unit from a
#'   model replication. Columns include information such as the number of
#'   particles used for the fitting algorithm and likelihood evaluation, and the
#'   parameters that the likelihood was evaluated at.
#' @export
#'
tidy_results = function(x, path = NA){
  UseMethod("tidy_results")
}

tidy_results_helper = function(
    ELL,
    path,
    N_fitr,
    Np,
    cf,
    np_eval,
    nreps_eval,
    block,
    obs_hash
){
  ELL |>
    measlespkg::tidy_pfilter_dfs() |>
    dplyr::mutate(
      path = path,
      N_fitr = N_fitr,
      np_fitr = Np,
      cooling_frac = cf,
      np_eval = np_eval,
      nreps_eval = nreps_eval,
      block = block,
      obs_hash = obs_hash
    ) |>
    dplyr::select(
      "path", "N_fitr", "np_fitr", "cooling_frac", "block", "np_eval",
      "nreps_eval", dplyr::everything()
    )
}

#' @rdname tidy_results
#' @export
tidy_results.EL_list = function(x, path = NA){
  ELL = x
  N_fitr = NA
  cf = NA
  block = NA
  Np = NA
  obs_hash = NA
  np_eval = ELL$np_pf
  nreps_eval = ELL$nreps
  tidy_results_helper(
    ELL = ELL,
    path = path,
    N_fitr = N_fitr,
    Np = Np,
    cf = cf,
    np_eval = np_eval,
    nreps_eval = nreps_eval,
    block = block,
    obs_hash = obs_hash
  )
}

#' @rdname tidy_results
#' @export
tidy_results.fit_results = function(x, path = NA){
  ### Backwards compatibility ###
  x = update_fit_results(x)
  FITR_O = x$fitr_out[[1]]
  ELL = x$EL_out
  ###
  if(inherits(FITR_O, "ibpfd_spatPomp")){
    N_fitr = FITR_O@Nbpf
    block = NA
    Np = mean(FITR_O@Np)
  }
  if(inherits(FITR_O, "mif2d.ppomp")){
    N_fitr = FITR_O@Nmif
    block = FITR_O@block
    Np = FITR_O@Np
  }
  obs_hash = rlang::hash(obs2(FITR_O))
  ###############################
  cf = FITR_O@cooling.fraction.50
  np_eval = ELL$np_pf
  nreps_eval = ELL$nreps
  tidy_results_helper(
    ELL = ELL,
    path = path,
    N_fitr = N_fitr,
    Np = Np,
    cf = cf,
    np_eval = np_eval,
    nreps_eval = nreps_eval,
    block = block,
    obs_hash = obs_hash
  )
}

