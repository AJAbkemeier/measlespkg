#' Duplicate top fits from `EL_list`
#'
#' @param x Object of class `EL_list`.
#' @param out_length Number of parameter sets to output. `top_n` should divide
#' `out_length`
#' @param top_n Number of top fits to duplicate.
#'
#' @return List of parameters in the form of `pparams()`.
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
    top_n = 1
  ){
  if(out_length %% top_n != 0){
    stop(
      "top_n should divide out_length",
      call. = FALSE
    )
  }
  if(ncol(x$ull) == 1){
    grabbed_params = grab_top_fits(x, top_n = top_n)$fits %>%
      dplyr::select(-logLik, -se)
  } else {
    grabbed_params = combine_top_fits(x, top_n = top_n)$fits %>%
      dplyr::select(-logLik, -se)
  }
  top_params = dplyr::slice(
    grabbed_params,
    rep(1:nrow(grabbed_params), each = out_length/top_n)
  )
  lapply(1:nrow(top_params), function(z){
    coef_to_pparams(top_params[z,])
  })
}
