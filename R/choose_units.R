#' Choose which units to use from data
#'
#' @param data List of data in the form of `twentycities`.
#' @param units Character vector of unit names.
#'
#' @return List of data in the form of `twentycities` containing only
#' observations associated with `units`.
#' @export
#'
#' @examples
#' choose_units(ur_measles, c("London", "Hastings", "Lees"))
choose_units = function(
    data,
    units
){
  out = lapply(seq_along(data), function(z){
    data[[z]][data[[z]]$unit %in% units,]
  })
  names(out) = names(data)
  out
}
