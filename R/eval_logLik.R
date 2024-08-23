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
    seed = NULL
){
  if(inherits(model_obj_list[[1]], "panelPomp")) pType = "panelPomp"
  if(inherits(model_obj_list[[1]], "spatPomp")) pType = "spatPomp"

  N_models = length(model_obj_list)
  units = switch(pType,
    panelPomp = names(model_obj_list[[1]]),
    spatPomp = model_obj_list[[1]]@unit_names
  )

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

  doParallel::registerDoParallel(cores = ncores)
  RNGkind("L'Ecuyer-CMRG")
  doRNG::registerDoRNG(seed)

  foreach_out = foreach::foreach(
    i = 1:nreps,
    .packages = pType
  ) %dopar% {
    lapply(model_obj_list, function(x){
      if(pType == "panelPomp"){
        out = panelPomp::pfilter(x, Np = np_pf)
        out = list(
          ull = panelPomp::unitLogLik(out),
          cll = sapply(
            tryCatch(out@unit_objects, error = function(x) out@unit.objects),
            function(u){
              u@cond.logLik
          }) |> t() |> `rownames<-`(units)
        )
      } else if (pType == "spatPomp"){
        out = spatPomp::bpfilter(
          x,
          Np = np_pf,
          block_size = block_size
        )
        out = list(
          ull = rowSums(out@block.cond.loglik),
          cll = out@block.cond.loglik
        )
        names(out$ull) = units
        rownames(out$cll) = units
      }
      out
    })
  }

  ull_matrices = lapply(1:N_models, function(i){
    lapply(1:nreps, function(j){
      foreach_out[[j]][[i]]$ull
    }) |> dplyr::bind_rows() |> as.matrix()
  })

  cllse_matrices = lapply(1:N_models, function(i){
    lapply(units, function(u){
      sapply(1:nreps, function(j){
        foreach_out[[j]][[i]]$cll[u,]
      }) |> apply(MARGIN = 1, FUN = pomp::logmeanexp, se = TRUE)
    }) |> `names<-`(units)
  })

  llse = sapply(1:N_models, function(i){
    if(pType == "panelPomp"){
      panelPomp::panel_logmeanexp(ull_matrices[[i]], MARGIN = 2, se = TRUE)
    } else if(pType == "spatPomp"){
      pomp::logmeanexp(rowSums(ull_matrices[[i]]), se = TRUE)
    }
  }) |> t()
  pf_logLik_frame[,1:2] = llse

  ullse = lapply(1:N_models, function(i){
    apply(ull_matrices[[i]], MARGIN = 2, FUN = pomp::logmeanexp, se = TRUE)
  }) |> t()

  ull = lapply(1:N_models, function(i){
    out = as.data.frame(ullse[[i]][1,, drop = FALSE])
    rownames(out) = NULL
    out
  }) |> dplyr::bind_rows()

  se = lapply(1:N_models, function(i){
    out = as.data.frame(ullse[[i]][2,, drop = FALSE])
    rownames(out) = NULL
    out
  }) |> dplyr::bind_rows()

  cll = lapply(units, function(u){
    sapply(1:N_models, function(i){
      cllse_matrices[[i]][[u]]["est",]
    }) |> t()
  })
  names(cll) = units
  cll_se = lapply(units, function(u){
    sapply(1:N_models, function(i){
      cllse_matrices[[i]][[u]]["se",]
    }) |> t()
  })
  names(cll_se) = units

  new_EL_list(
    fits = pf_logLik_frame,
    ull = ull,
    se = se,
    cll = cll,
    cll_se = cll_se,
    np_pf = np_pf,
    nreps = nreps
  )
}
