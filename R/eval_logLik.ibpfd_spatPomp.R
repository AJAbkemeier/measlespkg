#' Evaluate log likelihood of spatPomp model using the block particle filter
#'
#' @param model_obj_list List of model objects to evaluate log likelihood of.
#' @param block_size The number of spatial units per block.
#' @param ncores Number of cores to use for parallel computing.
#' @param np_pf Number of particles to use.
#' @param nreps Number of block particle filter repetitions.
#' @param seed Seed for particle filter. If NULL, does not set new seed.
#' @param divisor seed mod divisor*i is used to obtain seed for ith repetition.
#'   If NULL, does not set new seed.
#' @param return_pfilter_obj Should the returned list include the bpfilter
#'   object from the last repetition?
#'
#' @return A list containing the results of the likelihood evaluation. The `ull`
#'   and `se` data frames are not estimates of the actual `ull` and unit `se`,
#'   as the units are not independent.
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
  N_models = length(model_obj_list)
  units = as.data.frame(model_obj_list[[1]])$unit |> unique()
  U = length(units)

  pf_logLik_frame = data.frame(
    logLik = rep(0, N_models),
    se = rep(0, N_models)
  ) %>% cbind(
    rbind(t(sapply(model_obj_list, panelPomp::coef)))
  )

  pf_ull_list = vector(mode = "list", length = N_models)
  pf_se_list = vector("list", N_models)

  for(i in seq_along(model_obj_list)){
    doParallel::registerDoParallel(cores = ncores)
    seed_i = if(is.null(seed) | is.null(divisor)) NULL else (seed*i) %% divisor
    doRNG::registerDoRNG(seed_i)
    foreach::foreach(
      j = 1:nreps,
      .packages = "spatPomp"#.combine = c
    ) %dopar% {
      pfilter_obj = spatPomp::bpfilter(
        model_obj_list[[i]],
        Np = np_pf,
        block_size = block_size
      )
      #spatPomp::logLik(pfilter_obj)
      pfilter_obj@block.cond.loglik
    } -> block.cond.logLik_list
    logLik_vec = sapply(block.cond.logLik_list, sum)
    pf_logLik_frame[i, 1:2] = pomp::logmeanexp(logLik_vec, se = TRUE)
    pf_unitlogLik_matrix = lapply(block.cond.logLik_list, function(x){
      ull = rowSums(x)
      names(ull) = units
      ull
    }) |>
      dplyr::bind_rows()
    unit_calcs = apply(
      pf_unitlogLik_matrix,
      MARGIN = 2,
      FUN = pomp::logmeanexp,
      se = TRUE
    )
    rownames(unit_calcs)[[1]] = "loglik"
    pf_ull_list[[i]] = subset(
      unit_calcs,
      rownames(unit_calcs) == "loglik"
    ) |>
      as.data.frame()
    pf_se_list[[i]] = subset(unit_calcs, rownames(unit_calcs) == "se") |>
      as.data.frame()
  }
  pf_ull_frame = data.frame(dplyr::bind_rows(pf_ull_list))
  rownames(pf_ull_frame) = 1:nrow(pf_ull_frame)
  pf_se_frame = data.frame(dplyr::bind_rows(pf_se_list))
  rownames(pf_se_frame) = 1:nrow(pf_se_frame)
  spatEL_list = list(
    fits = pf_logLik_frame,
    ull = pf_ull_frame,
    se = pf_se_frame,
    np_pf = np_pf,
    nreps = nreps
  )
  if(return_pfilter_obj)
    spatEL_list = c(spatEL_list, pfilter_obj = pfilter_obj)
  spatEL_list
}
