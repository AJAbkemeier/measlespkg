#' Evaluate log likelihood of model using particle filter
#'
#' @param model_obj_list List of model objects to evaluate log likelihood of.
#' @param ncores Number of cores to use for parallel computing.
#' @param np_pf Number of particles to use.
#' @param nreps Number of particle filter repetitions.
#' @param seed Seed for particle filter. If NULL, does not set new seed.
#' @param divisor seed mod divisor*i is used to obtain seed for ith repetition.
#'   If NULL, does not set new seed.
#' @param return_n_pfilter_objs Number of pfilter objects to return.
#'
#'
#' @return Object of type `EL_list`, a list of data frames containing log
#'   likelihood and se estimates.
#' @export
#'
#' @examples
#' model_list = list(AK_model())
#' eval_logLik(model_list, ncores = 1, np_pf = 3, nreps = 2)
eval_logLik = function(
    model_obj_list,
    ncores,
    np_pf,
    nreps,
    seed = NULL,
    divisor = NULL,
    return_n_pfilter_objs = 0
){
  pf_logLik_frame = data.frame(
    logLik = rep(0, length(model_obj_list)),
    se = rep(0, length(model_obj_list))
  ) %>% cbind(
    rbind(t(sapply(model_obj_list, panelPomp::coef)))
  )

  pf_unitlogLik_list = vector("list", length(model_obj_list))
  pf_unitSE_list = vector("list", length(model_obj_list))

  for(i in seq_along(model_obj_list)){
    doParallel::registerDoParallel(cores = ncores)
    seed_i = if(is.null(seed) | is.null(divisor)) NULL else (seed*i) %% divisor
    doRNG::registerDoRNG(seed_i)
    foreach::foreach(
      j = 1:nreps,
      .packages = "panelPomp",
      .combine = c
    ) %dopar% {
      panelPomp::pfilter(model_obj_list[[i]], Np = np_pf)
    } -> pf_list
    if(nreps == 1) pf_list = list(pf_list)
    pf_unitlogLik_matrix = lapply(pf_list, panelPomp::unitlogLik) |>
      dplyr::bind_rows()
    pf_logLik_frame[i, 1:2] = panelPomp::panel_logmeanexp(
      pf_unitlogLik_matrix,
      MARGIN = 2,
      se = TRUE
    )
    unit_calcs = apply(
      pf_unitlogLik_matrix,
      MARGIN = 2,
      FUN = pomp::logmeanexp,
      se = TRUE
    )
    rownames(unit_calcs)[[1]] = "loglik"
    pf_unitlogLik_list[[i]] = subset(
      unit_calcs,
      rownames(unit_calcs) == "loglik"
    ) |>
      as.data.frame()
    pf_unitSE_list[[i]] = subset(unit_calcs, rownames(unit_calcs) == "se") |>
      as.data.frame()
  }
  pf_unitlogLik_frame = data.frame(dplyr::bind_rows(pf_unitlogLik_list))
  rownames(pf_unitlogLik_frame) = 1:nrow(pf_unitlogLik_frame)
  pf_unitSE_frame = data.frame(dplyr::bind_rows(pf_unitSE_list))
  rownames(pf_unitSE_frame) = 1:nrow(pf_unitSE_frame)
  pf_frames_list = new_EL_list(
    fits = pf_logLik_frame,
    ull = pf_unitlogLik_frame,
    se = pf_unitSE_frame,
    np_pf = np_pf,
    nreps = nreps
  )
  if(return_n_pfilter_objs > 0)
    pf_frames_list$pfilter_list = pf_list[1:return_n_pfilter_objs]
  pf_frames_list
}
