#' Convert `coef.spatPomp` to `pparams` format
#'
#' @param spatCoef Named numeric vector in format of `coef.spatPomp` object.
#' @param units Character vector of unit names.
#'
#' @return Parameters in the format of [panelPomp::pparams].
#' @export
#'
spatCoef_to_pparams = function(spatCoef, units){
  U = length(units)
  units_sorted = sort(units)
  spatCoef_sorted = spatCoef[sort(names(spatCoef))]
  param_names = names(spatCoef_sorted)[
    seq(from = 1, to = length(spatCoef_sorted), by = U)
  ] |>
    sapply(USE.NAMES = FALSE, function(x){
      substr(x, start = 1, stop = nchar(x) - 1)
    })
  specific_params = lapply(1:length(param_names), function(i){
    params = spatCoef_sorted[1:U + U*(i - 1)]
    names(params) = units_sorted
    params
  }) |>
    dplyr::bind_rows() |>
    as.data.frame()
  rownames(specific_params) = param_names
  list(
    shared = numeric(),
    specific = specific_params
  )
}
