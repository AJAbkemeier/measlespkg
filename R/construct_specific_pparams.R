#' Title
#'
#' @param pparams_df
#' @param coef_choices
#'
#' @return
#' @export
#'
#' @examples
construct_specific_pparams = function(pparams_df, coef_choices){
  out = pparams_df
  for(param in names(coef_choices)){
    out[param,] = coef_choices[[param]]
  }
  out
}
