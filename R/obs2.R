#' Extract the data matrix from a pomp or panelPomp object.
#'
#' @param x An object of class `pomp` or `panelPomp`.
#'
#' @return A matrix of observations with rows corresponding to units and columns
#'   corresponding to time points.
#' @export
#'
#' @examples
#' \dontrun{
#' obs2(AK_model())
#' }
obs2 = function(x){
  UseMethod("obs2")
}

#' @exportS3Method measlespkg::obs2
obs2.panelPomp = function(x){
  #tryCatch for backwards compatibility
  tryCatch(x@unit_objects, error = function(z) x@unit.objects) |>
    sapply(pomp::obs) |>
    t()
}

#' @exportS3Method measlespkg::obs2
obs2.pomp = function(x){
  pomp::obs(x)
}
