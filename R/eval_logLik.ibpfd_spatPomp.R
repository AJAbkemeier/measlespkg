#' Evaluate log likelihood of spatPomp model using the block particle filter
#'
#' @param model_obj_list List of model objects to evaluate log likelihood of.
#' @param block_size The number of spatial units per block.
#' @param ncores Number of cores to use for parallel computing.
#' @param np_pf Number of particles to use.
#' @param nreps Number of block particle filter repetitions.
#' @param seed Seed for particle filter. If NULL, does not set new seed.
#' @param divisor seed mod divisor*i is used to obtain seed for ith repetition.
#' If NULL, does not set new seed.
#' @param return_pfilter_obj Should the returned list include the bpfilter
#' object from the last repetition?
#'
#' @return A list containing the results of the likelihood evaluation.
#' @export
#'
eval_logLik.ibpfd_spatPomp = function(
    model_obj_list,
    block_size = 1,
    ncores,
    np_pf,
    nreps,
    seed = NULL,
    divisor = NULL,
    return_pfilter_obj = FALSE
){
  pf_logLik_frame = data.frame(
    logLik = rep(0, length(model_obj_list)),
    se = rep(0, length(model_obj_list))
  ) %>% cbind(
    rbind(t(sapply(model_obj_list, panelPomp::coef)))
  )

  for(i in seq_along(model_obj_list)){
    doParallel::registerDoParallel(cores = ncores)
    seed_i = if(is.null(seed) | is.null(divisor)) NULL else (seed*i) %% divisor
    doRNG::registerDoRNG(seed_i)
    foreach::foreach(
      j = 1:nreps,
      .packages = "spatPomp",
      .combine = c
    ) %dopar% {
      pfilter_obj = spatPomp::bpfilter(
        model_obj_list[[i]],
        Np = np_pf,
        block_size = block_size
      )
      spatPomp::logLik(pfilter_obj)
    } -> logLik_vec
    pf_logLik_frame[i, 1:2] = pomp::logmeanexp(logLik_vec, se = TRUE)
  }

  spatEL_list = list(
    fits = pf_logLik_frame,
    np_pf = np_pf,
    nreps = nreps
  )
  if(return_pfilter_obj)
    spatEL_list = c(spatEL_list, pfilter_obj = pfilter_obj)
  spatEL_list
}
