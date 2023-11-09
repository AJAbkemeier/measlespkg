#' Use centers and radii to sample initial parameters from box
#'
#' @description This is useful when you want to sample initial parameters
#' in the vicinity of a large number of specific parameters. Instead of
#' manually specifying bounds for `parameter1[unit1]`, `parameter1[unit2]`,
#' etc., which may have quite different values, you can specify vector of
#' centers (based on a past fit, for example) and then sample in an interval
#' around each one using radii for `parameter1`, `parameter2`, etc.
#'
#'
#' @param sh_rc `tbl` with `param`, `center`, and `radius` columns.
#' @param sp_c `data.frame` of specific parameter values in
#' format of `pparams()` output.
#' @param sp_r `tbl` with `param` and `radius` columns.
#' @param n_draws Number of initial parameter sets to draw.
#' @param buffer The minimum space required between the boundaries of the
#' sampling box and the boundaries of the possible values for the parameters.
#' @param pos_params Character vector of parameters which must be positive.
#' @param unit_interval_params Character vector of parameters which must be
#' between 0 and 1.
#'
#' @return A list of parameter sets in the `pparams()` format.
#' @export
#'
#' @examples
#' \dontrun{
#' AK_mod = AK_model()
#' sh_rc = tibble::tribble(
#'   ~param, ~center, ~radius,
#'  "gamma1",    -0.63695, 0.05,
#'  "gamma0",    4.61215,    0.5,
#'  "mu",        0.02,        0
#' )
#' specific_radii = tibble::tribble(
#'   ~param, ~radius,
#'   "R0",        6,
#'   "rho",       0.05,
#'   "sigmaSE",   6,
#'   "amplitude", 0.08,
#'   "S_0",       0.01,
#'   "E_0",       0.0001,
#'   "I_0",       0.001,
#'   "R_0",       0.01,
#'   "sigma",     10,
#'   "iota",      0.5,
#'   "psi",       0.4,
#'   "alpha",     0.02,
#'   "cohort",    0.1,
#'   "gamma",     30
#' )
#' sample_initial_pparams_rc(
#'   sh_rc = sh_rc,
#'   sp_c = panelPomp::pparams(AK_mod)$specific,
#'   sp_r = specific_radii,
#'   n_draws = 3
#' )
#' }
sample_initial_pparams_rc = function(
    sh_rc,
    sp_c,
    sp_r,
    n_draws,
    buffer = 5e-8,
    pos_params = c("R0", "mu", "sigmaSE", "iota", "sigma", "psi", "alpha"),
    unit_interval_params = c("cohort", "amplitude", "S_0", "E_0", "I_0", "R_0",
                             "rho")
){
  specific_box_specs_f = function(sp_c, sp_r){
    specific_centers_tbl = sp_c |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "param")
    specific_box = sp_r |>
      dplyr::right_join(specific_centers_tbl, by = "param") |>
      tidyr::pivot_longer(
        cols = c(-"param", -"radius"),
        values_to = "center",
        names_to = "unit"
      ) |>
      dplyr::arrange(.data$unit)
    specific_box
  }
  specific_box_specs = specific_box_specs_f(sp_c, sp_r)
  adjust_params = function(x){
    x |>
      dplyr::mutate(lower = ifelse(
        .data$param %in% c(pos_params, unit_interval_params) &
          .data$center - .data$radius < buffer,
        buffer,
        .data$center - .data$radius
      )) |>
      dplyr::mutate(upper = ifelse(
        .data$param %in% unit_interval_params &
          .data$lower + 2*.data$radius > 1 - buffer,
        1 - buffer,
        .data$lower + 2*.data$radius
      ))
  }
  shared_bounds = adjust_params(sh_rc) |>
    dplyr::select("param", "lower", "upper")
  specific_bounds = adjust_params(specific_box_specs) |>
    dplyr::select("param", "unit", "lower", "upper") |>
    tidyr::unite("param", "unit", sep = "[", col = "param[unit]") |>
    dplyr::mutate(`param[unit]` = paste0(.data$`param[unit]`, "]"))

  to_named_vec = function(x, name_col, val_col){
    named_vec = x[[val_col]]
    names(named_vec) = x[[name_col]]
    named_vec
  }
  initial_parameters_tbl = dplyr::bind_cols(
    pomp::runif_design(
      lower = to_named_vec(shared_bounds, "param", "lower"),
      upper = to_named_vec(shared_bounds, "param", "upper"),
      nseq = n_draws
    ),
    pomp::runif_design(
      lower = to_named_vec(specific_bounds, "param[unit]", "lower"),
      upper = to_named_vec(specific_bounds, "param[unit]", "upper"),
      nseq = n_draws
    )
  )

  lapply(1:nrow(initial_parameters_tbl), function(z)
    coef_to_pparams(initial_parameters_tbl[z,])
  )
}
