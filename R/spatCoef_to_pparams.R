#' Convert `coef.spatPomp` to `pparams` format
#'
#' @param spatCoef `coef.spatPomp` object
#' @param units Character vector of unit names
#'
#' @return Parameters in the format of `pparams`.
#' @export
#'
spatCoef_to_pparams = function(spatCoef, units){
  U = length(units)
  units_sorted = sort(units)
  param_names = names(spatCoef)[seq(from = 1, to = length(spatCoef), by = U)] |>
    sapply(USE.NAMES = FALSE, function(x){
      substr(x, start = 1, stop = nchar(x) - 1)
    })
  specific_params = lapply(1:length(param_names), function(i){
    params = spatCoef[1:U + U*(i - 1)]
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
