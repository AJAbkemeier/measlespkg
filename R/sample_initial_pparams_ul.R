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
#' AK_mod = AK_model()
#' shared_bounds = tibble::tribble(
#' ~param, ~lower,     ~upper,
#' "mu",        0.02,     0.02
#' )
#' specific_bounds = tibble::tribble(
#'   ~param,       ~lower,        ~upper,
#'   "R0",             10,            60,
#'   "rho",           0.1,           0.9,
#'   "sigmaSE",      0.04,           0.1,
#'   "amplitude",     0.1,           0.6,
#'   "S_0",          0.01,          0.07,
#'   "E_0",      0.000004,        0.0001,
#'   "I_0",      0.000003,         0.001,
#'   "R_0",           0.9,          0.99,
#'   "sigma",          25,           100,
#'   "iota",        0.004,             3,
#'   "psi",          0.05,             3,
#'   "alpha",       0.935,          1.05,
#'   "cohort",        0.1,           0.7,
#'   "gamma",          25,           320
#' )
#' sample_initial_pparams_ul(
#'   shared_box_specs = shared_bounds,
#'   specific_box_specs = specific_bounds,
#'   units = names(AK_mod),
#'   n_draws = 3
#' )
sample_initial_pparams_ul = function(
    shared_box_specs,
    specific_box_specs,
    units,
    n_draws
){
  shared_box_specs_rc = shared_box_specs |>
    dplyr::transmute(
      param = .data$param,
      center = (.data$upper + .data$lower)/2,
      radius = (.data$upper - .data$lower)/2
    )
  radii_tbl = specific_box_specs |>
    dplyr::transmute(
      param = .data$param,
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
