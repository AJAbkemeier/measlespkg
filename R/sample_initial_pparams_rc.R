#' Use centers and radii to sample initial parameters from box
#'
#' @param shared_box_specs `tbl` with `param`, `center`, and `radius` columns.
#' @param specific_pparams_df `data.frame` of specific parameter values in
#' format of pparams.
#' @param radii_tbl `tbl` with `param` and `radius` columns.
#' @param n_draws Number of initial parameter sets to draw.
#' @param buffer The minimum space required between the boundaries of the
#' sampling box and the boundaries of the possible values for the parameters.
#' @param pos_params Character vector of parameters which must be positive.
#' @param unit_interval_params Character vector of parameters which must be
#' between 0 and 1.
#'
#' @return A list of parameters sets in the `pparams()` format.
#' @export
#'
#' @examples
sample_initial_pparams_rc = function(
    shared_box_specs,
    specific_pparams_df,
    radii_tbl,
    n_draws,
    buffer = 5e-8,
    pos_params = c("R0", "mu", "rho", "sigmaSE"),
    unit_interval_params = c("cohort", "amplitude", "S_0", "E_0", "I_0", "R_0")
){
  specific_box_specs_f = function(specific_pparams_df, radii_tbl){
    specific_centers_tbl = specific_pparams_df %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "param")
    specific_box = radii_tbl %>%
      dplyr::right_join(specific_centers_tbl, by = "param") %>%
      tidyr::pivot_longer(
        c(-param, -radius),
        values_to = "center",
        names_to = "unit"
      ) %>%
      dplyr::arrange(unit)
    specific_box
  }
  specific_box_specs = specific_box_specs_f(specific_pparams_df, radii_tbl)
  adjust_params = function(x){
    x %>%
      dplyr::mutate(lower = ifelse(
        param %in% c(pos_params, unit_interval_params) &
          center - radius < buffer,
        buffer,
        center - radius
      )) %>%
      dplyr::mutate(upper = ifelse(
        param %in% unit_interval_params & lower + 2*radius > 1 - buffer,
        1 - buffer,
        lower + 2*radius
      ))
  }
  shared_bounds = adjust_params(shared_box_specs) %>%
    dplyr::select(param, lower, upper)
  specific_bounds = adjust_params(specific_box_specs) %>%
    dplyr::select(.data$param, .data$unit, .data$lower, .data$upper) %>%
    tidyr::unite(.data$param, .data$unit, sep = "[", col = "param[unit]") %>%
    dplyr::mutate(`param[unit]` = paste0(.data$`param[unit]`, "]"))

  to_named_vec = function(x, name_col, val_col){
    named_vec = x[[val_col]]
    names(named_vec) = x[[name_col]]
    named_vec
  }
  initial_parameters_tbl = dplyr::bind_cols(
    runif_design(
      lower = to_named_vec(shared_bounds, "param", "lower"),
      upper = to_named_vec(shared_bounds, "param", "upper"),
      nseq = n_draws
    ),
    runif_design(
      lower = to_named_vec(specific_bounds, "param[unit]", "lower"),
      upper = to_named_vec(specific_bounds, "param[unit]", "upper"),
      nseq = n_draws
    )
  )

  lapply(1:nrow(initial_parameters_tbl), function(z)
    coef_to_pparams(initial_parameters_tbl[z,])
  )
}
