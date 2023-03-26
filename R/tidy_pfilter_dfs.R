#' Create tidy data frame from `eval_logLik()` output.
#'
#' @param x Object of class `EL_list`.
#'
#' @return Data frame composed from information in `x` data in tidy format.
#' @export
#'
#' @examples
#' model_list = list(AK_model())
#' EL_out = eval_logLik(model_list, ncores = 1, np_pf = 3, nreps = 2)
#' tidy_pfilter_dfs(EL_out)
tidy_pfilter_dfs = function(x){
  lapply(1:nrow(x$fits), function(z){
    tidy_LL_df = coef_to_pparams(x$fits[z,-c(1,2)])$specific |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "unit") |>
      dplyr::mutate(rep = z)
    tidy_LL_df = coef_to_pparams(x$fits[z,-c(1,2)])$shared |>
      tibble::enframe() |>
      tidyr::pivot_wider() |>
      dplyr::bind_cols(tidy_LL_df)
    tidy_ull_df = subset(x$ull, subset = rownames(x$ull) == z) |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "unit") |>
      dplyr::rename(ull = .data$V1)
    tidy_se_df = subset(x$se, subset = rownames(x$se) == z) |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "unit") |>
      dplyr::rename(se = .data$V1)
    tidy_df = dplyr::left_join(tidy_ull_df, tidy_LL_df, by = "unit") |>
      dplyr::left_join(tidy_se_df, by = "unit") |>
      dplyr::select(
        .data$rep, .data$unit, .data$ull, .data$se, dplyr::everything()
      )
    tidy_df
  }) |>
    dplyr::bind_rows() -> tidy_coef_df
  tidy_coef_df
}
