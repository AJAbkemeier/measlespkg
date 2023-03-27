#' Combine parameter estimates with best unit log likelihoods
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
#' EL_out$fits
#' combine_top_fits(EL_out, top_n = 2)
combine_top_fits = function(
    x,
    top_n = 1,
    se_penalty = 0
){
  unit_names = colnames(x$ull)
  best_shared = coef_to_pparams(
    dplyr::select(x$fits[1,], -.data$logLik, -.data$se)
  )[[1]]
  shared_names = names(best_shared)
  is_unique = sapply(shared_names, function(z)
    length(unique(x$fits[[z]])) == 1
  )
  if(!all(is_unique)){
    warning("Fits do not all have same shared parameters. Grabbing shared parameters from top row.")
  }
  fits_cols = vector(length = length(unit_names), mode = "list")
  ull_cols = fits_cols
  se_cols = fits_cols
  for(z in seq_along(unit_names)){
    un = unit_names[[z]]
    score = x$ull[[un]] - se_penalty*x$se[[un]]
    ranking = order(score, decreasing = TRUE)[1:top_n]
    fits_cols[[z]] = x$fits[ranking,] %>%
      dplyr::select(dplyr::contains(un))
    ull_cols[[z]] = x$ull[ranking, un]
    names(ull_cols)[[z]] = un
    se_cols[[z]] = x$se[ranking, un]
    names(se_cols)[[z]] = un
  }
  fits = dplyr::bind_cols(fits_cols)
  ull = dplyr::bind_cols(ull_cols)
  se = dplyr::bind_cols(se_cols)
  for(shared_name in names(best_shared)){
    fits = fits %>%
      dplyr::mutate(SHARED = best_shared[[shared_name]])
    colnames(fits)[[ncol(fits)]] = shared_name
  }
  fits = fits %>%
    dplyr::mutate(
      logLik = rowSums(ull),
      se = sapply(1:nrow(se), function(z)
        sqrt(sum(se[z,]^2))
      )
    ) %>%
    dplyr::select(.data$logLik, .data$se, dplyr::everything())
  new_EL_list(
    fits = fits,
    ull = ull,
    se = se,
    np_pf = x$np_pf,
    nreps = x$nreps
  )
}
