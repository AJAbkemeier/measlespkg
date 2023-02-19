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
  shared = c(mu = AK_mles$mu[[1]])
  specific = AK_mles %>%
    dplyr::select(-.data$loglik, -.data$loglik.sd, -.data$mu, -.data$delay) %>%
    tibble::column_to_rownames(var = "town") %>%
    t()
  pparams = list(shared = shared, specific = specific)
  make_measlesPomp(
    twentycities,
    starting_pparams = pparams,
    model = model_mechanics_001(),
    AK_interp = TRUE,
  )
}
