#' Create tidy data frame from saved `eval_logLik()` output.
#'
#' @param EL_out List shaped like the output of `eval_logLik()`.
#'
#' @return Tibble of `EL_out` data in tidy format.
#' @export
#'
#' @examples
#' model_list = list(AK_model())
#' EL_out = eval_logLik(model_list, ncores = 1, np_pf = 3, nreps = 2)
#' tidy_pfilter_dfs(EL_out)
tidy_pfilter_dfs = function(EL_list){
  LL_df = EL_list$fits
  ULL_df = EL_list$ull
  lapply(1:nrow(LL_df), function(x){
    coef_to_pparams(LL_df[x,])$specific %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "unit") %>%
      dplyr::mutate(rep = x) -> tidy_LL_df
    tidy_df = subset(ULL_df, subset = rownames(ULL_df) == x) %>%
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
