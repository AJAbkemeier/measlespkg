#' Convert `coef.spatPomp` to `pparams` format
#'
#' @param spatCoef Named numeric vector in format of `coef.spatPomp` object.
#'   This function assumes that parameters are named according to the scheme
#'   "param1, param2, param3,..." where "param" is the base parameter name and
#'   doesn't end with a digit other than 0. Hence, R0 and S_0 would be valid
#'   base parameter names but gamma1 would not be.
#' @param units Character vector of unit names in the order they are enumerated
#'   in `spatCoef`.
#'
#' @return Parameters in the format of `panelPomp::coef()` with
#'   `format = "list"`.
#' @export
#'
spatCoef_to_pparams = function(spatCoef, units){
  U = length(units)
  units_sorted = sort(units)
  # This sorts parameter names alphabetically so that the input order doesn't
  # matter, but it messes with the order of the enumeration because, e.g,
  # param11 comes before param2.
  spatCoef_sorted = spatCoef[sort(names(spatCoef))]
  # Strings passed to sapply are of the form "param1", so removing the last
  # character obtains "param," the base parameter name, which shouldn't end in a
  # digit other than 0 according to the naming assumption.
  param_names = names(spatCoef_sorted)[
    seq(from = 1, to = length(spatCoef_sorted), by = U)
  ] |>
    sapply(USE.NAMES = FALSE, function(x){
      substr(x, start = 1, stop = nchar(x) - 1)
    })
  name_assumption_violated = any(grepl("[1-9]$", x = param_names))
  if(name_assumption_violated){
    stop("One or more base parameter names ends with a digit other than 0.")
  }

  spatCoef_enumeration = names(spatCoef_sorted)[1:U] |>
    gsub(param_names[[1]], "", x = _) |>
    as.numeric() |>
    lapply(1:length(param_names), function(x, a){
      a + U*(x - 1)
    }, a = _) |>
    unlist()
  spatCoef_sorted_final = spatCoef_sorted[order(spatCoef_enumeration)]
  specific_params = lapply(1:length(param_names), function(i){
    params = spatCoef_sorted_final[1:U + U*(i - 1)]
    names(params) = units
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
