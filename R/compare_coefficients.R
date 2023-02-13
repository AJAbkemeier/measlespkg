compare_coefficients = function(old_coef, new_coef, print_plots = TRUE, 
                                save_dir = NULL, width = 16, height = 10,
                                xlimits = c(-1, 1.5)){
  new_coef = new_coef[order(names(new_coef))]
  old_coef = old_coef[order(names(old_coef))]
  if(sum(names(new_coef) != names(old_coef))>0) stop("Coef names do not match")
  rel_diff = (new_coef-old_coef)/abs(old_coef)
  
  rel_diff_tibble = tibble(
    rel_diff = rel_diff, 
    specific_param = names(rel_diff),
    param = gsub("\\[.*","",names(rel_diff)), 
    large = ifelse(rel_diff > xlimits[[2]], specific_param, NA),
    small = ifelse(rel_diff < xlimits[[1]], specific_param, NA)
  ) 
  
  coef_plot = ggplot(data = rel_diff_tibble, 
         mapping = aes(y = specific_param, x = rel_diff, color = param)) + 
    geom_point() +
    theme(axis.text = element_text(size = 4)) + 
    xlab("(new_coef-old_coef)/abs(old_coef)") + 
    ylab("Parameter name") + 
    ggtitle("Relative difference between He10 and best new model params")
  
  coef_plot_zoomed = ggplot(data = rel_diff_tibble, 
         mapping = aes(y = specific_param, x = rel_diff, color = param)) + 
    geom_point() +
    geom_hline(aes(yintercept = large),
               color = "red", size = 0.2) +
    geom_hline(aes(yintercept = small),
               color = "blue", size = 0.2) +
    geom_text(aes(y = names(rel_diff), x = xlimits[[1]], label = rel_diff),
              size = 2) + 
    coord_cartesian(xlim = xlimits) + 
    theme(axis.text = element_text(size = 4)) + 
    xlab("(new_coef-old_coef)/abs(old_coef)") + 
    ylab("Parameter name") + 
    ggtitle("Relative difference between He10 and best new model params")
  
  if(print_plots){
    print(coef_plot)
    print(coef_plot_zoomed)
  }
  if(!is.null(save_dir)){
    dir.create(save_dir)
    ggsave(filename = paste0("coef_plot.png"), 
           plot = coef_plot, path = save_dir, 
           width = width, height = height)
    ggsave(filename = paste0("coef_plot_zoomed.png"), 
           plot = coef_plot_zoomed, path = save_dir, 
           width = width, height = height)
  }
}
