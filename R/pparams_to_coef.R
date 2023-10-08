#' Convert list of `pparams` into `coef` format
#'
#' @param pparams List of parameters in format of [panelPomp::pparams].
#'
#' @return Named vector of parameters in format used by `coef()`.
#' @export
#'
pparams_to_coef = function(pparams){
  shp_names = names(pparams$shared)
  spp_names = rownames(pparams$specific)
  unit_names = colnames(pparams$specific)
  total_names = c(
    shp_names,
    unlist(lapply(spp_names, function(x,un) paste0(x,"[",un,"]"),unit_names))
  )
  coef_out = rep(0, length(total_names))
  names(coef_out) = total_names
  for(p in shp_names){
    coef_out[p] = pparams$shared[[p]]
  }
  for(p in spp_names){
    for(un in unit_names){
      coef_out[paste0(p,"[",un,"]")] = pparams$specific[p,un]
    }
  }
  coef_out
}
