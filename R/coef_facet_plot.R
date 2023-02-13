coef_facet_plot = function(pparams_new, pparams_old, lmp = NULL, exclude = "mu"){
  pop_1950 = c(28930.000, 1115110.989, 292602.867, 442600.000, 244328.078,
               39130.000, 10512.639, 2171.000, 65690.000, 300126.070, 
               507798.358, 4234.329, 800130.806, 3389620.000, 703149.415, 
               6409.000, 18183.686, 305514.658, 10970.000, 513866.597)
  names(pop_1950) = c("Bedwellty", "Birmingham", "Bradford", "Bristol",
                      "Cardiff", "Consett", "Dalton.in.Furness", "Halesworth",
                      "Hastings", "Hull", "Leeds", "Lees", "Liverpool",
                      "London", "Manchester", "Mold", "Northwich", "Nottingham",       
                      "Oswestry", "Sheffield")
  std_log_pop_1950 = (log(pop_1950) - mean(log(pop_1950)))/sd(log(pop_1950))
  std_log_pop_tibble = tibble(pop_1950 = std_log_pop_1950,
                              unit = names(std_log_pop_1950))
  if(!is.null(lmp)){
    new_lmp = sapply(lmp, function(x)
      exp(pparams_new[[1]][[paste0(x, 0)]]+std_log_pop_1950*pparams_new[[1]][[paste0(x, 1)]])
    ) |> 
      t() |> 
      as.data.frame()
  }
  new_shared = cbind(replicate(
    length(pop_1950), 
    pparams_new[[1]][!(sapply(c(lmp, exclude), function(x)
      grepl(x, names(scratch_pparams[[1]]))
    ) |> as.matrix() |> rowSums() |> as.logical())]
  ))
  if(length(new_shared) == 0){
    colnames(new_shared) = names(pop_1950)
    new_long = pparams_new[[2]] %>%
      rbind(new_shared)
  } else {
    new_long = pparams_new[[2]]
  }
  if(!is.null(lmp)){
    new_long = new_long %>% rbind(new_lmp)
  }
  new_long = new_long %>%
    rownames_to_column(var = "parameter") %>%
    pivot_longer(names_to = "unit", cols = !parameter, 
                 values_to = "new_value")
  old_long = pparams_old[[2]] %>%
    as.data.frame() %>%
    rownames_to_column(var = "parameter") %>%
    pivot_longer(names_to = "unit", cols = !parameter, 
                 values_to = "old_value")
  combined_pparams = new_long %>%
    left_join(old_long, by = c("unit", "parameter")) %>%
    left_join(std_log_pop_tibble, by = "unit") %>%
    mutate(relative_diff = (new_value - old_value)/abs(old_value))
  
  ggplot(combined_pparams, mapping = aes(x = pop_1950, y = relative_diff)) +
    geom_point() + 
    facet_wrap(vars(parameter)) +
    xlab("std_log_pop_1950") + 
    ylab("(new_coef - old_coef)/abs(old_coef)") + 
    geom_hline(yintercept = 0)
}
