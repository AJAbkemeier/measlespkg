#' Evaluate the log likelihood of a model using `pfilter()`
#'
#' @param model_obj_list List of `panelPomp` or `spatPomp` objects to evaluate
#'   the log likelihood of.
#' @param block_size The number of spatial units per block. Only used when
#'   evaluating are `spatPomp` models. (NOTE: function will break for any block
#'   size other than 1; this will be fixed when choosing other block sizes seems
#'   worthwhile.)
#' @param ncores Number of cores to use for parallel computing.
#' @param np_pf Number of particles to use.
#' @param nreps Number of particle filter repetitions.
#' @param seed Seed for particle filter. If NULL, does not set new seed.
#' @param divisor `seed` mod `divisor*i` is used to obtain seed for ith
#'   repetition. If NULL, does not set new seed.
#' @param return_n_pfilter_objs Number of `pfilter` objects to return.
#'
#'
#' @return Object of type `EL_list`, a list of data frames containing log
#'   likelihood and standard error estimates.
#'
#'   Note that the unit log likelihoods estimated for `spatPomp` objects are not
#'   real unit log likelihoods given that the units are dependent, but they may
#'   still be useful when evaluating model fit.
#'
#'   For `panelPomp`, log likelihood estimates are obtained by using
#'   [panelPomp::panel_logmeanexp()] over the `pfilter()` log likelihood
#'   replications. For `spatPomp`, log likelihood estimates are obtained by
#'   using [pomp::logmeanexp()] over the `pfilter()` log likelihood
#'   replications. For both model types, unit log likelihood estimates are
#'   obrained by using `logmeanexp()` over the unit log likelihood replications
#'   for each unit, and conditional log likelihood estimates are obtained by
#'   using `logmeanexp()` over the condtional log likelihood replications for
#'   each unit and time point.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' model_list = list(AK_model())
#' eval_logLik(model_list, ncores = 1, np_pf = 3, nreps = 2)
#' }
eval_logLik = function(
    model_obj_list,
    block_size = 1,
    ncores,
    np_pf,
    nreps,
    seed = NULL,
    divisor = NULL,
    return_n_pfilter_objs = 0
){
  N_models = length(model_obj_list)
  pf_logLik_frame = data.frame(
    logLik = rep(0, N_models),
    se = rep(0, N_models)
  ) |>
  cbind(
    rbind(t(sapply(model_obj_list, panelPomp::coef)))
  )

  pf_unitlogLik_list = vector("list", N_models)
  pf_unitSE_list = vector("list", N_models)
  pf_cll_list = vector("list", N_models)

  for(i in 1:N_models){
    ELS_out = eval_logLik_single(
      model_obj = model_obj_list[[i]],
      block_size = block_size,
      ncores = ncores,
      np_pf = np_pf,
      nreps = nreps,
      seed = if(is.null(seed)) NULL else seed*i,
      divisor = divisor,
      return_n_pfilter_objs = return_n_pfilter_objs
    )
    pf_logLik_frame[i, 1:2] = ELS_out$logLikSE
    pf_unitlogLik_list[[i]] = ELS_out$pf_ull
    pf_unitSE_list[[i]] = ELS_out$pf_se
    pf_cll_list[[i]] = ELS_out$cll_calcs
  }

  pf_unitlogLik_frame = data.frame(dplyr::bind_rows(pf_unitlogLik_list))
  rownames(pf_unitlogLik_frame) = 1:nrow(pf_unitlogLik_frame)

  pf_unitSE_frame = data.frame(dplyr::bind_rows(pf_unitSE_list))
  rownames(pf_unitSE_frame) = 1:nrow(pf_unitSE_frame)

  units = colnames(pf_unitlogLik_frame)
  cll = lapply(units, function(u){
    sapply(1:N_models, function(i){
      pf_cll_list[[i]][[u]]["est",]
    }) |> t()
  })
  names(cll) = units
  cll_se = lapply(units, function(u){
    sapply(1:N_models, function(i){
      pf_cll_list[[i]][[u]]["se",]
    }) |> t()
  })
  names(cll_se) = units

  pf_frames_list = new_EL_list(
    fits = pf_logLik_frame,
    ull = pf_unitlogLik_frame,
    se = pf_unitSE_frame,
    cll = cll,
    cll_se = cll_se,
    np_pf = np_pf,
    nreps = nreps
  )
  if(return_n_pfilter_objs > 0)
    pf_frames_list$pfilter_list = ELS_out$pf_list[1:return_n_pfilter_objs]
  pf_frames_list
}

eval_logLik_single = function(
    model_obj,
    ncores,
    np_pf,
    nreps,
    seed,
    divisor,
    return_n_pfilter_objs,
    ...
){
  UseMethod("eval_logLik_single")
}

eval_logLik_single.panelPomp = function(
    model_obj,
    ncores,
    np_pf,
    nreps,
    seed,
    divisor,
    return_n_pfilter_objs,
    ...
){
  units = names(model_obj)
  doParallel::registerDoParallel(cores = ncores)
  seed_i = if(is.null(seed) | is.null(divisor)) NULL else seed %% divisor
  RNGkind("L'Ecuyer-CMRG")
  doRNG::registerDoRNG(seed_i)

  foreach::foreach(
    j = 1:nreps,
    .packages = "panelPomp"
  ) %dopar% {
    out = panelPomp::pfilter(model_obj, Np = np_pf)
    if(return_n_pfilter_objs == 0){
      out = list(
        ull = panelPomp::unitlogLik(out),
        cll = lapply(seq_along(units), function(u){
          out@unit.objects[[u]]@cond.logLik
        })
      )
    }
    out
  } -> pf_list

  pf_unitlogLik_matrix = if(return_n_pfilter_objs > 0){
    lapply(pf_list, panelPomp::unitlogLik) |>
      dplyr::bind_rows()
  } else {
    lapply(pf_list, function(x) x$ull) |>
      dplyr::bind_rows()
  }

  logLikSE = panelPomp::panel_logmeanexp(
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

  pf_ull = subset(
    unit_calcs,
    rownames(unit_calcs) == "loglik"
  ) |>
    as.data.frame()

  pf_se = subset(unit_calcs, rownames(unit_calcs) == "se") |>
    as.data.frame()

  cll_calcs = if(return_n_pfilter_objs > 0){
    lapply(units, function(u){
      sapply(seq_along(pf_list), function(x){
        pf_list[[x]]@unit.objects[[u]]@cond.logLik
      }) |>
        apply(MARGIN = 1, FUN = pomp::logmeanexp, se = TRUE)
    })
  } else {
    lapply(seq_along(units), function(u){
      sapply(seq_along(pf_list), function(x){
        pf_list[[x]]$cll[[u]]
      }) |>
        apply(MARGIN = 1, FUN = pomp::logmeanexp, se = TRUE)
    })
  }
  names(cll_calcs) = units

  pf_list = if(return_n_pfilter_objs > 0){
    pf_list[1:return_n_pfilter_objs]
  } else {
    NULL
  }

  list(
    logLikSE = logLikSE,
    pf_ull = pf_ull,
    pf_se = pf_se,
    cll_calcs = cll_calcs,
    pf_list = pf_list
  )
}

eval_logLik_single.spatPomp = function(
    model_obj,
    block_size,
    ncores,
    np_pf,
    nreps,
    seed,
    divisor,
    return_n_pfilter_objs,
    ...
){
  units = as.data.frame(model_obj)$unit |> unique()
  doParallel::registerDoParallel(cores = ncores)
  seed_i = if(is.null(seed) | is.null(divisor)) NULL else seed %% divisor
  RNGkind("L'Ecuyer-CMRG")
  doRNG::registerDoRNG(seed_i)
  foreach::foreach(
    j = 1:nreps,
    .packages = "spatPomp"
  ) %dopar% {
    out = spatPomp::bpfilter(
      model_obj,
      Np = np_pf,
      block_size = block_size
    )
    if(return_n_pfilter_objs == 0){
      out = out@block.cond.loglik
    }
    out
  } -> pf_list
  if(return_n_pfilter_objs == 0){
    block.cond.logLik_list = pf_list
    pf_list = NULL
  } else {
    block.cond.logLik_list = lapply(pf_list, function(x) x@block.cond.loglik)
    pf_list = pf_list[1:return_n_pfilter_objs]
  }
  logLik_vec = sapply(block.cond.logLik_list, sum)
  logLikSE = pomp::logmeanexp(logLik_vec, se = TRUE)

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

  pf_ull = subset(
    unit_calcs,
    rownames(unit_calcs) == "loglik"
  ) |>
    as.data.frame()

  pf_se = subset(unit_calcs, rownames(unit_calcs) == "se") |>
    as.data.frame()

  cll_calcs = lapply(seq_along(units), function(u){
    sapply(seq_along(block.cond.logLik_list), function(x){
      block.cond.logLik_list[[x]][u,]
    }) |>
      apply(MARGIN = 1, FUN = pomp::logmeanexp, se = TRUE)
  })
  names(cll_calcs) = units

  list(
    logLikSE = logLikSE,
    pf_ull = pf_ull,
    pf_se = pf_se,
    cll_calcs = cll_calcs,
    pf_list = pf_list
  )
}
