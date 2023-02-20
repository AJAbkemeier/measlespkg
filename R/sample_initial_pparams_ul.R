#' Use upper and lower bounds to sample initial parameters from box
#'
#' @param shared_box_specs `tbl` with `param`, `lower`, and `upper` columns.
#' @param specific_box_specs `tbl` with `param`, `lower`, and `upper` columns.
#' format of pparams.
#' @param units Character vector of unit names.
#' @param n_draws Number of initial parameter sets to draw.
#'
#' @return A list of parameters sets in the `pparams()` format.
#' @export
#'
#' @examples
sample_initial_pparams_ul = function(
    shared_box_specs,
    specific_box_specs,
    units,
    n_draws
){
  shared_box_specs_rc = shared_box_specs %>%
    dplyr::transmute(
      param = .data$param,
      center = (.data$upper + .data$lower)/2,
      radius = (.data$upper - .data$lower)/2
    )
  radii_tbl = specific_box_specs %>%
    dplyr::transmute(
      param = param,
      radius = (.data$upper - .data$lower)/2
    )
  centers = (specific_box_specs$lower + specific_box_specs$upper)/2
  specific_pparams = matrix(
    centers,
    nrow = length(centers),
    ncol = length(units)
  )
  colnames(specific_pparams) = units
  rownames(specific_pparams) = specific_box_specs$param
  sample_initial_pparams_rc(
    shared_box_specs = shared_box_specs_rc,
    specific_pparams_df = as.data.frame(specific_pparams),
    radii_tbl = radii_tbl,
    n_draws = n_draws,
    buffer = 0
  )
}
