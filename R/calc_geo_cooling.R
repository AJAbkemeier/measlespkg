#' Calculate amount by which rw.sd is cooled in geometric cooling scheme.
#'
#' @param cooling_fraction_50 Fraction which rw.sd is multiplied by after 50
#' iterations.
#' @param n Time step number we want the cumulative cooling fraction for.
#' @param m Iteration number we want the cumulative cooling fraction for.
#' @param N Length of time series.
#'
#' @return A numeric vector of length one.
#' @export
#'
#' @examples
#' calc_geo_cooling(1/2, 1, m = 51, 730)
calc_geo_cooling = function(cooling_fraction_50, n = 1, m = 1, N = 730){
  cooling_fraction_50^((n-1+(m-1)*N)/(50*N))
}
