#' Extract the data matrix from a pomp/panelPomp/spatPomp object.
#'
#' @param x An object of class `pomp`, `panelPomp`, or `spatPomp`.
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
  if(inherits(x, "panelPomp")){
    out = panelPomp::unit_objects(x) |> sapply(pomp::obs) |> t()
  } else {
    out = pomp::obs(x)
  }
  out
}
