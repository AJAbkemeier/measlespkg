plot_traces = function(mif2_alt_list, plot_shared = ".ALL", 
                       plot_specific = ".ALL", print_plots = TRUE, 
                       save_dir = NULL, width = 16, height= 10){
  stitch_traces = function(mif2_alt_list){
    nalts = length(mif2_alt_list)
    nreps = length(mif2_alt_list[[1]])
    nalt_iter = mif2_alt_list[[1]][[1]]@Nmif
    traces_list = vector("list", nreps)
    for(i in seq_along(traces_list)){
      traces_list_i = lapply(1:nalts, function(x) 
        traces(mif2_alt_list[[x]][[i]])
      )
      for(j in seq_along(traces_list_i)){
        if(j > 1){
          traces_list_i[[j]] = traces_list_i[[j]][2:(nalt_iter+1),]
        }
      }
      traces_list[[i]] = do.call(rbind, traces_list_i)
    }
    traces_list
  }
  nalts = length(mif2_alt_list)
  nreps = length(mif2_alt_list[[1]])
  niter = mif2_alt_list[[1]][[1]]@Nmif*nalts
  traces_list = stitch_traces(mif2_alt_list)
  
  trace_names = colnames(traces_list[[1]])
  specific_trace_indices = grep("\\[", trace_names)
  plot_sh = trace_names[-specific_trace_indices]
  plot_sp = unique(gsub("\\[.*", "", trace_names[specific_trace_indices]))
  if(!is.null(plot_shared))
    if(!setequal(plot_shared, ".ALL")) plot_sh = plot_shared
  if(!is.null(plot_specific))
    if(!setequal(plot_specific, ".ALL")) plot_sp = plot_specific
  
  if(!is.null(save_dir)) dir.create(save_dir)
  
  # Plot shared traces
  if(!is.null(plot_shared)){
    for(plot_col in plot_sh){
      long_data = as_tibble(
        sapply(1:nreps, function(x) traces_list[[x]][,plot_col])
      ) %>% 
        mutate(iter = 0:niter) %>% 
        pivot_longer(cols = 1:nreps)
      
      traces_ggplot = ggplot(
          long_data, aes(x = iter, y = value, color = name)
        ) + 
        geom_line() + 
        ylab(plot_col) +
        guides(color = "none")
      if(print_plots) print(traces_ggplot)
      if(!is.null(save_dir)){
        ggsave(filename = paste0("trace_plot_",plot_col,".png"), 
               plot = traces_ggplot, path = save_dir, 
               width = width, height = height)
      }
    }
  }
  # Plot specific traces
  if(!is.null(plot_specific)){
    for(plot_param in plot_sp){
      plot_units = names(mif2_alt_list[[1]][[1]])
      plot_cols = paste0(plot_param,"[",plot_units,"]")
      long_data = lapply(1:nreps, function(x){
        as_tibble(traces_list[[x]]) %>%
          select(plot_cols) %>% 
          mutate(repl = x) %>%
          mutate(iter = 0:niter)
      }) %>% 
        bind_rows() %>% 
        pivot_longer(cols = plot_cols)
      
      traces_ggplot = ggplot(long_data, aes(x = iter, y = value, 
                                            color = paste0(name,repl))) + 
        geom_line(size = 0.5, alpha = 0.25) + 
        guides(color = "none") + 
        facet_wrap(vars(name), scales = "free") 
      if(print_plots) print(traces_ggplot)
      if(!is.null(save_dir)){
        ggsave(filename = paste0("trace_plot_",plot_param,".png"), 
               plot = traces_ggplot, path = save_dir, 
               width = width, height = height)
      }
    }
  }
}
