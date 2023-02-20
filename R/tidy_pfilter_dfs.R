#' Create tidy data frame from saved `eval_logLik()` output.
#'
#' @param x Object of class `EL_list`.
#'
#' @return Tibble of `x` data in tidy format.
#' @export
#'
#' @examples
#' model_list = list(AK_model())
#' EL_out = eval_logLik(model_list, ncores = 1, np_pf = 3, nreps = 2)
#' tidy_pfilter_dfs(EL_out)
tidy_pfilter_dfs = function(x){
  LL_df = x$fits
  ULL_df = x$ull
  lapply(1:nrow(LL_df), function(z){
    coef_to_pparams(LL_df[z,])$specific %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "unit") %>%
      dplyr::mutate(rep = z) -> tidy_LL_df
    tidy_df = subset(ULL_df, subset = rownames(ULL_df) == z) %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "unit") %>%
      dplyr::mutate(ull = .[[2]]) %>%
      dplyr::select(.data$unit, .data$ull) %>%
      dplyr::left_join(tidy_LL_df, by = "unit")
    tidy_df
  }) %>%
    dplyr::bind_rows() -> tidy_coef_df
  tidy_coef_df
}
