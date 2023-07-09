#' Create tidy data frame from `eval_logLik()` output.
#'
#' @param x Object of class `EL_list`.
#'
#' @return Data frame composed from information in `x` data in tidy format.
#' @export
#'
#' @examples
#' \dontrun{
#' model_list = list(AK_model())
#' EL_out = eval_logLik(model_list, ncores = 1, np_pf = 3, nreps = 2)
#' tidy_pfilter_dfs(EL_out)
#' }
tidy_pfilter_dfs = function(x){
  stopifnot(class(x) == "EL_list")
  # Handle case where fits are from spatPomp model
  if(any(grepl("\\[", colnames(x$fits)))){
    conversion_function = coef_to_pparams
  } else {
    units = colnames(x$ull)
    conversion_function = function(x) spatCoef_to_pparams(x, units)
  }
  lapply(1:nrow(x$fits), function(z){
    z_pparams = conversion_function(x$fits[z,-c(1, 2)])
    tidy_LL_df = z_pparams$specific |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "unit") |>
      dplyr::mutate(rep = z)
    if(length(z_pparams$shared) > 0){
      tidy_LL_df = z_pparams$shared |>
        tibble::enframe() |>
        tidyr::pivot_wider() |>
        dplyr::bind_cols(tidy_LL_df)
    }
    tidy_ull_df = subset(x$ull, subset = rownames(x$ull) == z) |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "unit") |>
      dplyr::rename(ull = 2)
    tidy_se_df = subset(x$se, subset = rownames(x$se) == z) |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "unit") |>
      dplyr::rename(se = 2)
    tidy_df = dplyr::left_join(tidy_ull_df, tidy_LL_df, by = "unit") |>
      dplyr::left_join(tidy_se_df, by = "unit") |>
      dplyr::mutate(total_ll = x$fits$logLik[[z]], total_se = x$fits$se[[z]]) |>
      dplyr::select(
        "rep", "total_ll", "total_se", "unit", "ull", "se", dplyr::everything()
      )
    tidy_df
  }) |>
    dplyr::bind_rows() -> tidy_coef_df
  tidy_coef_df
}
