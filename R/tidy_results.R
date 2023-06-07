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
    block
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
      block = block
    ) |>
    dplyr::select(
      path, N_fitr, np_fitr, cooling_frac, block, np_eval, nreps_eval,
      dplyr::everything()
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
    block = block
  )
}

#' @rdname tidy_results
#' @export
tidy_results.fit_results = function(x, path = NA){
  ### Backwards compatibility ###
  if(length(x$EL_out) == 1){
    ELL = x$EL_out[[1]]
  } else {
    ELL = x$EL_out
  }
  if(names(x)[[1]] %in% c("mif2_out", "ibpf_out")){
    FITR_O = x[[1]][[1]]
  } else {
    FITR_O = x$fitr_out[[1]]
  }
  if(class(FITR_O) == "ibpfd_spatPomp"){
    N_fitr = FITR_O@Nbpf
    block = NA
    Np = mean(FITR_O@Np)
  }
  if(class(FITR_O) == "mif2d.ppomp"){
    N_fitr = FITR_O@Nmif
    block = FITR_O@block
    Np = FITR_O@Np
  }
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
    block = block
  )
}

