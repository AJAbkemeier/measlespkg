#' Duplicate top fits from `EL_list`
#'
#' @param x Object of class `EL_list`.
#' @param out_length Number of parameter sets to output.
#' @param top_n Number of top fits to duplicate.
#' @param combine Boolean specifying whether best specific fits should be
#'   combined.
#' @param units Character vector of unit names, which is necessary when
#'   duplicating the parameters of a `spatPomp` object. If the parameters belong
#'   to a `panelPomp` object, leave as `NULL`.
#'
#' @return List of parameters in the form of [panelPomp::pparams()].
#' @export
#'
#' @examples
#' AK_mod = AK_model()
#' EL_out = eval_logLik(
#'   list(AK_mod, AK_mod, AK_mod),
#'   ncores = 1,
#'   np_pf = 1,
#'   nreps = 2
#' )
#' duplicate_top_pparams(EL_out, out_length = 6, top_n = 2)
duplicate_top_pparams = function(
    x,
    out_length,
    top_n = 1,
    combine = FALSE,
    units = NULL
){
  if(out_length %% top_n != 0){
    stop(
      "top_n should divide out_length",
      call. = FALSE
    )
  }
  if(ncol(x$ull) == 1 | combine == FALSE){
    grabbed_params = grab_top_fits(x, top_n = top_n)$fits %>%
      dplyr::select(-.data$logLik, -.data$se)
  } else {
    grabbed_params = combine_top_fits(x, top_n = top_n)$fits %>%
      dplyr::select(-.data$logLik, -.data$se)
  }
  top_params = dplyr::slice(
    grabbed_params,
    rep_len(1:nrow(grabbed_params), length.out = out_length)
  )
  lapply(1:nrow(top_params), function(z){
    if(is.null(units))
      coef_to_pparams(top_params[z,])
    else
      spatCoef_to_pparams(top_params[z,], units)
  })
}
