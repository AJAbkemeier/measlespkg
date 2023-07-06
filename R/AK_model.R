#' Create panelPomp object using AK's re-fitting of He10's model.
#'
#' @description AK's model is like He10's model EXCEPT that population and
#' births are interpolated using smooth splines and the interpolated covariates
#' are shifted +0.5 years. In contrast, He10 uses linear interpolation and
#' makes no shift. In addition, AK's MLE's are different.
#'
#' @return panelPomp object.
#' @export
#'
#' @examples
#' AK_model()
AK_model = function(){
  shared = c(mu = measlespkg::AK_mles$mu[[1]])
  specific = measlespkg::AK_mles |>
    dplyr::select(-"loglik", -"loglik.sd", -"mu", -"delay") |>
    tibble::column_to_rownames(var = "town") |>
    t()
  pparams = list(shared = shared, specific = specific)
  make_measlesPomp(
    measlespkg::clean_twentycities(),
    starting_pparams = pparams,
    model = model_mechanics_001(),
    interp_method = "shifted_splines",
  )
}
