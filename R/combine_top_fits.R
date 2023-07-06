#' Combine parameter estimates with best unit log likelihoods
#'
#' @param x Object of class `EL_list`.
#' @param top_n Number of rows to grab.
#' @param se_penalty Penalizes log likelihood in ranking based on
#'   `se*se_penalty`.
#' @param is_spat Indicates whether parameters in `x` are for a spatPOMP model.
#'   Function will fail to return desired results if wrong.
#'
#'
#' @return Object of class `EL_list`.
#' @export
#'
#' @examples
#' \dontrun{
#' AK_mod = AK_model()
#' EL_out = eval_logLik(
#'   list(AK_mod, AK_mod, AK_mod),
#'   ncores = 1,
#'   np_pf = 1,
#'   nreps = 2
#' )
#' EL_out$fits
#' combine_top_fits(EL_out, top_n = 2)
#' }
combine_top_fits = function(
    x,
    top_n = 1,
    se_penalty = 0,
    is_spat = FALSE
){
  unit_names = colnames(x$ull)
  best_fit = dplyr::select(
    x$fits[order(x$fits$logLik, decreasing = TRUE),][1,],
    -"logLik", -"se"
  )
  if(is_spat){
    best_shared = spatCoef_to_pparams(best_fit, units = unit_names)[[1]]
  } else {
    best_shared = coef_to_pparams(best_fit)[[1]]
  }
  shared_names = names(best_shared)
  is_unique = sapply(shared_names, function(z)
    length(unique(x$fits[[z]])) == 1
  )
  if(!all(is_unique)){
    warning("Fits do not all have the same shared parameters. Grabbing shared parameters from the best overall fit.")
  }
  fits_cols = vector(length = length(unit_names), mode = "list")
  ull_cols = fits_cols
  se_cols = fits_cols
  cll = fits_cols
  names(cll) = unit_names
  cll_se = fits_cols
  names(cll_se) = unit_names
  for(z in seq_along(unit_names)){
    un = unit_names[[z]]
    score = x$ull[[un]] - se_penalty*x$se[[un]]
    ranking = order(score, decreasing = TRUE)[1:top_n]
    if(is_spat){
      # TODO Locate parameters in safer way.
      fits_cols[[z]] = x$fits[ranking,] |>
        dplyr::select(grep(paste0("[^1-9]",z,"$"), colnames(x$fits)))
    } else {
      fits_cols[[z]] = x$fits[ranking,] |> dplyr::select(dplyr::contains(un))
    }
    ull_cols[[z]] = x$ull[ranking, un]
    names(ull_cols)[[z]] = un
    se_cols[[z]] = x$se[ranking, un]
    names(se_cols)[[z]] = un
    cll[[unit_names[[z]]]] = x$cll[[unit_names[[z]]]][ranking, , drop = FALSE]
    cll_se[[unit_names[[z]]]] =
      x$cll_se[[unit_names[[z]]]][ranking, , drop = FALSE]
  }
  fits = dplyr::bind_cols(fits_cols)
  ull = dplyr::bind_cols(ull_cols)
  se = dplyr::bind_cols(se_cols)
  for(shared_name in names(best_shared)){
    fits[[shared_name]] = best_shared[[shared_name]]
  }
  fits = fits |>
    dplyr::mutate(
      logLik = rowSums(ull),
      se = sapply(1:nrow(se), function(z)
        sqrt(sum(se[z,]^2))
      )
    )
  fits = fits[colnames(x$fits)]
  ull = ull[colnames(x$ull)]
  se = se[colnames(x$se)]
  cll = cll[names(x$cll)]
  cll_se = cll_se[names(x$cll_se)]
  new_EL_list(
    fits = fits,
    ull = ull,
    se = se,
    cll = cll,
    cll_se = cll_se,
    np_pf = x$np_pf,
    nreps = x$nreps
  )
}
