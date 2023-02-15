#' Title
#'
#' @param pf_logLik_frame
#' @param top_n
#'
#' @return
#' @export
#'
#' @examples
grab_top_params = function(pf_logLik_frame, top_n = 1){
  pf_logLik_frame[order(pf_logLik_frame[[1]], decreasing = TRUE),][1:top_n,]
}
