#' Convert `coef.spatPomp` to `pparams` format
#'
#' @param spatCoef Named numeric vector in format of `coef.spatPomp` object.
#' @param units Character vector of unit names in the order they are enumerated
#'   in `spatCoef`.
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
  spatCoef_enumeration = names(spatCoef_sorted)[1:U] |>
    gsub(param_names[[1]], "", x = _) |>
    as.numeric() |>
    lapply(1:length(param_names), function(x, a){
      a + U*(x - 1)
    }, a = _) |>
    unlist()
  spatCoef_sorted_final = spatCoef_sorted[order(spatCoef_enumeration)]
  units_sorted_final = units_sorted[order(spatCoef_enumeration[1:U])]
  specific_params = lapply(1:length(param_names), function(i){
    params = spatCoef_sorted_final[1:U + U*(i - 1)]
    names(params) = units_sorted_final
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
