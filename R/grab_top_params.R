#' Grab rows for top `n` fits based on log likelihood
#'
#' @param EL_list List output from `eval_logLik`.
#' @param top_n How many rows to grab.
#' @param se_penalty Penalizes log likelihood in ranking based on
#' `se*se_penalty`.
#'
#' @return Data frame of rows for top `n` fits.
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
grab_top_params = function(EL_list, top_n = 1, se_penalty = 0){
  score = EL_list$fits$logLik - se_penalty*EL_list$fits$se
  ranking = order(score, decreasing = TRUE)
  EL_list$fits[,][1:top_n,]
  list(
    fits = EL_list$fits[ranking,][1:top_n,],
    ull = EL_list$ull[ranking,][1:top_n,],
    se = EL_list$se[ranking,][1:top_n,]
  )
}
