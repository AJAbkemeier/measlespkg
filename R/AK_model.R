#' Create panelPomp object using Aaron King's re-fitting of the He10 model.
#'
#' @description Aaron King's model is like He et al. 2010 model EXCEPT that
#'   population and births are interpolated using smooth splines and the
#'   interpolated covariates are shifted +0.5 years. In contrast, He et al. use
#'   linear interpolation and make no shift. In addition, Aaron King's MLE's are
#'   different.
#'
#' @return panelPomp object.
#' @export
#'
#' @source <https://kingaa.github.io/sbied/measles/index.html>
#'
#' @examples
#' \dontrun{
#' AK_model()
#' }
AK_model = function(){
  shared = c(mu = measlespkg::AK_mles$mu[[1]])
  specific = measlespkg::AK_mles |>
    dplyr::select(-"loglik", -"loglik.sd", -"mu", -"delay") |>
    tibble::column_to_rownames(var = "town") |>
    t()
  pparams = list(shared = shared, specific = specific)
  make_measlesPomp(
    data = measlespkg::clean_twentycities(),
    starting_pparams = pparams,
    model = model_mechanics_001(),
    interp_method = "shifted_splines",
  )
}
