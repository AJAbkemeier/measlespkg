tidy_pfilter_dfs = function(LL_df, ULL_df){
  lapply(1:nrow(LL_df), function(x){
    coef_to_pparams(LL_df[x,])$specific %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column(var = "unit") %>%
      mutate(rep = x) -> tidy_LL_df
    tidy_df = subset(ULL_df, subset = rownames(ULL_df) == x) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column(var = "unit") %>%
      mutate(ull = .[[2]]) %>% 
      select(unit, ull) %>%
      left_join(tidy_LL_df, by = "unit")
    tidy_df
  }) %>%
    bind_rows() -> tidy_coef_df
  tidy_coef_df
}
