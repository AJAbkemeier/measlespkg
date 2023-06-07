#' Make new fit_results object
#'
#' @param fitr_out List of `mif2d.ppomp` or `ibpfd_spatPomp` objects.
#' @param EL_out `EL_list` object.
#'
#' @return List of class `fit_results` with first entry `fitr_out` and second
#'   entry `EL_out`. `fitr_out` is a list of `mif2d.ppomp` or `ibpfd_spatPomp`
#'   objects, and `EL_out` is an `EL_list`.
#'
new_fit_results = function(fitr_out, EL_out){
  stopifnot(is.list(fitr_out))
  stopifnot(class(EL_out) == "EL_list")
  out = list(
    fitr_out = fitr_out,
    EL_out = EL_out
  )
  structure(out, class = "fit_results")
}
