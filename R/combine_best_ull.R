#' Title
#'
#' @param ll_frame
#' @param ull_frame
#' @param se_frame
#'
#' @return
#' @export
#'
#' @examples
combine_best_ull = function(ll_frame, ull_frame, se_frame){
  # Note: order of output does not quite match that of coef(); probably fine
  n_cols = ncol(ull_frame)
  best_ull_indices = sapply(1:n_cols,
    function(x) which(ull_frame[,x] == max(ull_frame[,x]))
  )
  best_ll = sapply(1:n_cols,
    function(x) ull_frame[best_ull_indices[[x]],x]
  ) |>
    sum()
  best_se = sapply(1:n_cols,
    function(x) se_frame[best_ull_indices[[x]],x]
  )^2 |>
    sum() |>
    sqrt()
  output = c(best_ll, best_se)
  names(output) = c("logLik", "se")

  sh_frame = ll_frame %>%
    as_tibble() %>%
    select(!contains("[") & !contains("log") & !contains("se"))
  best_shared = as.numeric(sh_frame[1,])
  names(best_shared) = colnames(sh_frame)

  sp_frame = ll_frame %>%
    as_tibble() %>%
    select(contains("["))

  coef_list = lapply(1:nrow(sp_frame), function(x) sp_frame[x,])
  for(i in seq_along(coef_list)){
    coef_list[[i]] = coef_list[[i]] %>%
      pivot_longer(everything(), names_to = "sp") %>%
      separate(sp, sep = "\\[", into = c("param", "unit")) %>%
      mutate(unit = gsub("\\]", "", unit)) %>%
      pivot_wider(names_from = unit, values_from = value) %>%
      column_to_rownames(var = "param")
  }
  best_coefficients = sapply(1:n_cols,
    function(x) coef_list[[best_ull_indices[[x]]]][,x]
  )
  rownames(best_coefficients) = rownames(coef_list[[1]])
  colnames(best_coefficients) = colnames(coef_list[[1]])
  best_coefficients_long = best_coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "param") %>%
    as_tibble() %>%
    pivot_longer(!matches("param"), names_to = "unit") %>%
    unite(param, unit, sep = "[", col = "sp") %>%
    mutate(sp = paste0(sp, "]"))
  best_specific = best_coefficients_long$value
  names(best_specific) = best_coefficients_long$sp

  output = c(output, best_shared, best_specific)
  output[colnames(ll_frame)]
}
