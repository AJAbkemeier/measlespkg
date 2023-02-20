#' Grab rows for top `n` fits based on log likelihood
#'
#' @param x Object of class `EL_list`.
#' @param top_n Number of rows to grab.
#' @param se_penalty Penalizes log likelihood in ranking based on
#' `se*se_penalty`.
#'
#' @return Object of class `EL_list`.
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
#' EL_out$fits[, 1:4]
#' grab_top_params(EL_out)$fits[, 1:4]
grab_top_fits = function(
    x,
    top_n = 1,
    se_penalty = 0
  ){
  score = x$fits$logLik - se_penalty*x$fits$se
  ranking = order(score, decreasing = TRUE)
  new_EL_list(
    fits = x$fits[ranking,][1:top_n,],
    ull = x$ull[ranking,][1:top_n,],
    se = x$se[ranking,][1:top_n,]
  )
}
