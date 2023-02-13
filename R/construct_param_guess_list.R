construct_param_guess_list = function(shared_box_specs, specific_box_specs,
nseq, buffer = 5e-8, seed = NULL,
pos_params = c("R0", "mu", "rho", "sigmaSE"),
unit_interval_params = c("cohort", "amplitude", "S_0", "E_0", "I_0", "R_0")){
  shared_bounds = shared_box_specs %>%
    mutate(lower = ifelse(
      param %in% c(pos_params, unit_interval_params) &
        center - radius < buffer, 
      buffer, 
      center - radius
    )) %>%
    mutate(upper = ifelse(
      param %in% unit_interval_params & lower + 2*radius > 1 - buffer,
      1 - buffer,
      lower + 2*radius
    )) %>% 
    select(param, lower, upper)
  
  specific_bounds = specific_box_specs %>%
    mutate(lower = ifelse(
      param %in% c(pos_params, unit_interval_params) &
        center - radius < buffer, 
      buffer, 
      center - radius
    )) %>%
    mutate(upper = ifelse(
      param %in% unit_interval_params & lower + 2*radius > 1 - buffer,
      1 - buffer,
      lower + 2*radius
    )) %>% 
    select(param, unit, lower, upper) %>% 
    unite(param, unit, sep = ":", col = "param")
  
  to_named_vec = function(x, name_col, val_col){
    named_vec = x[[val_col]]
    names(named_vec) = x[[name_col]]
    named_vec
  }
  
  if(!is.null(seed))
    set.seed(seed)
  
  shared_guesses = runif_design(
    lower = to_named_vec(shared_bounds, "param", "lower"),
    upper = to_named_vec(shared_bounds, "param", "upper"),
    nseq = nseq
  )
  specific_guesses = runif_design(
    lower = to_named_vec(specific_bounds, "param", "lower"),
    upper = to_named_vec(specific_bounds, "param", "upper"),
    nseq = nseq
  )
  
  shared_guess_list = vector("list", nseq)
  specific_guess_list = vector("list", nseq)
  for(i in seq_along(specific_guess_list)){
    shared_guess_list[[i]] = as.numeric(shared_guesses[i,])
    names(shared_guess_list[[i]]) = colnames(shared_guesses)
    
    specific_guess_list[[i]] = as.data.frame(t(specific_guesses)) %>%
      rownames_to_column() %>%
      separate(rowname, sep = ":", into = c("param", "unit")) %>% 
      select(param, unit, paste0("V", i)) %>% 
      pivot_wider(names_from = unit, values_from = paste0("V", i)) %>% 
      column_to_rownames("param")
  }
  list(shared = shared_guess_list, specific = specific_guess_list)
}
