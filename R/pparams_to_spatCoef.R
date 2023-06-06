#' Convert list of `pparams` into `coef.spatPomp` format
#'
#' @param pparams List of parameters in format of [panelPomp::pparams()].
#'
#' @return Named vector of parameters in format used by `coef.spatPomp`.
#' @export
#'
pparams_to_spatCoef = function(pparams){
  shp_names = names(pparams$shared)
  spp_names = rownames(pparams$specific)
  U = ncol(pparams$specific)
  total_names = c(
    unlist(lapply(shp_names, function(x) paste0(x,1))),
    unlist(lapply(spp_names, function(x,U) paste0(x,1:U),U))
  )
  coef_out = rep(0, length(total_names))
  names(coef_out) = total_names
  for(p in shp_names){
    coef_out[paste0(p,1)] = pparams$shared[[p]]
  }
  spp_sorted = pparams$specific[sort(colnames(pparams$specific))]
  for(p in spp_names){
    vec = as.numeric(spp_sorted[p,])
    coef_out[paste0(p,1:U)] = vec
  }
  coef_out
}
