#' Create tidy data frame from saved pfilter output.
#'
#' @param LL_df
#' @param ULL_df
#'
#' @return
#' @export
#'
#' @examples
tidy_pfilter_dfs = function(LL_df, ULL_df){
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
      dplyr::select(unit, ull) %>%
      dplyr::left_join(tidy_LL_df, by = "unit")
    tidy_df
  }) %>%
    dplyr::bind_rows() -> tidy_coef_df
  tidy_coef_df
}
