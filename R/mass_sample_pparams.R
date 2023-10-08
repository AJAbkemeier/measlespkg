#' Quickly evaluate log likelihood at parameter sets and return best sets.
#'
#' @param model_obj panelPomp object to run `pfilter()` on.
#' @param pparams_list List of parameters in the format of `pparams()` to
#'   evaluate the log likelihood of `model_obj` at.
#' @param Np Number of particles to use.
#' @param Nt Number of time points to use.
#' @param rank_scheme How to rank the parameter sets.
#' @param top_n Number of best parameter sets to output.
#' @param ncores Number of cores to use.
#'
#' @return A list of parameters in the format of `pparams()`.
#' @export
mass_sample_pparams = function(
    model_obj,
    pparams_list,
    Np = 1,
    Nt = 1,
    rank_scheme = c("total_ll", "ull"),
    top_n = 1,
    ncores = 1
){
  stopifnot(rank_scheme[[1]] %in% c("total_ll", "ull"))
  units = names(model_obj)

  for(i in seq_along(model_obj)){
    model_obj@unit.objects[[i]]@times = model_obj@unit.objects[[i]]@times[1:Nt]
    obs_matrix = t(pomp::obs(model_obj@unit.objects[[i]])[1:Nt])
    rownames(obs_matrix) = rownames(model_obj@unit.objects[[i]]@data)
    model_obj@unit.objects[[i]]@data = obs_matrix
  }

  doParallel::registerDoParallel(cores = ncores)
  RNGkind("L'Ecuyer-CMRG")
  doRNG::registerDoRNG()
  j = 0 # Deals with devtools::check() message
  foreach::foreach(
    j = 1:length(pparams_list),
    .combine = rbind,
    .packages = "panelPomp"
  ) %dopar% {
    panelPomp::coef(model_obj) = pparams_to_coef(pparams_list[[j]])
    pp = panelPomp::pfilter(model_obj, Np = Np)
    panelPomp::unitlogLik(pp)
  } -> ull_matrix

  if(rank_scheme[[1]] == "total_ll"){
    out = pparams_list[order(rowSums(ull_matrix), decreasing = TRUE)[1:top_n]]
  } else if(rank_scheme[[1]] == "ull"){
    order_mat = apply(ull_matrix, MARGIN = 2, FUN = order)[1:top_n,]
    out = lapply(1:top_n, function(x){
      specific = lapply(units, function(u){
        rep = order_mat[x,u]
        pparams_list[[rep]]$specific[,u]
      }) |> do.call(cbind, args = _)
      rownames(specific) = rownames(pparams_list[[1]]$specific)
      colnames(specific) = colnames(pparams_list[[1]]$specific)

      shared = lapply(units, function(u){
        rep = order_mat[x,u]
        pparams_list[[rep]]$shared
      }) |> do.call(rbind, args = _) |> colMeans()

      list(shared = shared, specific = specific)
    })
  }
  out
}
