#' Title
#'
#' @param pf_logLik_frame
#' @param pf_unitlogLik_frame
#' @param top_n
#'
#' @return
#' @export
#'
#' @examples
grab_top_unit_params = function(pf_logLik_frame, pf_unitlogLik_frame, top_n = 1){
  unit_names = coef_to_pparams(pf_logLik_frame[1,-(1:2)]) %>%
    .[[2]] %>%
    colnames()
  best_shared = coef_to_pparams(pf_logLik_frame[1,-(1:2)])[[1]]
  lapply(unit_names, function(x){
      top_indices = order(pf_unitlogLik_frame[[x]], decreasing = TRUE)[1:top_n]
      pf_logLik_frame[top_indices,] %>%
        select(contains(x))
    }
  ) %>%
    bind_cols() -> out
  for(shared_name in names(best_shared)){
    out = out %>%
      mutate(SHARED = best_shared[[shared_name]])
    colnames(out)[[ncol(out)]] = shared_name
  }
  out
}
