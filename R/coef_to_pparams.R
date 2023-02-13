coef_to_pparams = function(coef){
  coef_tibble = tibble(sp = names(coef), value = as.numeric(coef)) %>%
    separate(sp, into = c("param", "unit"), sep = "\\[", fill = "right") %>%
    mutate(unit = gsub(pattern = "\\]", "", x = unit))
  shared_tibble = filter(coef_tibble, is.na(unit))
  specific_tibble = filter(coef_tibble, !is.na(unit))
  shared_params = shared_tibble$value
  names(shared_params) = shared_tibble$param
  
  specific_params = specific_tibble %>%
    pivot_wider(names_from = unit, values_from = value) %>% 
    column_to_rownames(var = "param")
  
  list(shared = shared_params, specific = specific_params)
}
