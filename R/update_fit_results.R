#' Update structure of older versions of fit_results objects
#'
#' @param x `fit_results` object to be updated.
#'
#' @return Object of class `fit_results` with structure compatible with more
#'   recent versions of this package
#'
update_fit_results = function(x){
  if(names(x)[[1]] %in% c("mif2_out", "ibpf_out")){
    names(x)[[1]] = "fitr_out"
  }
  observations = obs2(x$fitr_out[[1]])
  units = rownames(observations)
  N = ncol(observations)
  if(inherits(x$EL_out[[1]], "EL_list")){
    x$EL_out = x$EL_out[[1]]
  }
  if(is.null(x$EL_out$cll)){
    x$EL_out$cll = lapply(units, function(z){
      matrix(NA, nrow = length(units), ncol = N)
    })
    names(x$EL_out$cll) = units
  }
  if(is.null(x$EL_out$cll_se)){
    x$EL_out$cll_se = lapply(units, function(z){
      matrix(NA, nrow = length(units), ncol = N)
    })
    names(x$EL_out$cll_se) = units
  }
  if(is.null(x$EL_out$np_pf)) x$EL_out$np_pf = NA
  if(is.null(x$EL_out$nreps)) x$EL_out$nreps = NA
  x$EL_out = x$EL_out[c("fits", "ull", "se", "cll", "cll_se", "np_pf", "nreps")]
  class(x$EL_out) = "EL_list"
  x
}
