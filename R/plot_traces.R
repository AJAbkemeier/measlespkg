#' Plot traces from `mif2` objects
#'
#' @param mif2_list List of `mif2` objects.
#' @param plot_shared String specifying which shared value traces should be
#' plotted. This includes log-likelihoods and shared parameters. Set to `".ALL"`
#' to plot all shared value traces.
#' @param plot_specific String specifying which unit-specific value traces
#' should be plotted. This includes unit log-likelihoods and specific
#' parameters. Set to `".ALL"` to plot all unit-specific value traces.
#' @param print_plots Boolean specifying whether plots should be printed.
#' @param save_dir String specifying where plots should be saved. No plots are
#' saved if `NULL`.
#' @param width Width of plots.
#' @param height Height of plots.
#' @param log_y Boolean specifying whether y-axis should be scaled by log base
#' 10.
#'
#' @return Returns `NULL`
#' @export
#'
plot_traces = function(
    mif2_list,
    plot_shared = ".ALL",
    plot_specific = ".ALL",
    print_plots = TRUE,
    save_dir = NULL,
    width = 16,
    height = 10,
    log_y = FALSE
){
  nreps = length(mif2_list)
  niter = mif2_list[[1]]@Nmif
  traces_list = lapply(1:nreps, function(i)
    pomp::traces(mif2_list[[i]])
  )

  trace_names = colnames(traces_list[[1]])
  specific_trace_indices = grep("\\[", trace_names)
  plot_sh = trace_names[-specific_trace_indices]
  plot_sp = unique(gsub("\\[.*", "", trace_names[specific_trace_indices]))
  if(!is.null(plot_shared))
    if(!setequal(plot_shared, ".ALL")) plot_sh = plot_shared
  if(!is.null(plot_specific))
    if(!setequal(plot_specific, ".ALL")) plot_sp = plot_specific

  if(!is.null(save_dir)){
    if(!dir.exists(save_dir))
      dir.create(save_dir)
  }
  # Plot shared traces
  if(!is.null(plot_shared)){
    for(plot_col in plot_sh){
      long_data = dplyr::as_tibble(
        sapply(1:nreps, function(x) traces_list[[x]][,plot_col])
      ) %>%
        dplyr::mutate(iter = 0:niter) %>%
        tidyr::pivot_longer(cols = 1:nreps)

      traces_ggplot = ggplot2::ggplot(
          long_data,
          ggplot2::aes(x = .data$iter, y = .data$value, color = .data$name)
        ) +
        ggplot2::geom_line() +
        ggplot2::ylab(plot_col) +
        ggplot2::guides(color = "none")
      if(log_y)
        traces_ggplot = traces_ggplot + ggplot2::scale_y_log10()
      if(print_plots) print(traces_ggplot)
      if(!is.null(save_dir)){
        ggplot2::ggsave(
          filename = paste0("trace_plot_", plot_col, ".png"),
          plot = traces_ggplot,
          path = save_dir,
          width = width,
          height = height
        )
      }
    }
  }
  # Plot specific traces
  if(!is.null(plot_specific)){
    for(plot_param in plot_sp){
      plot_units = names(mif2_list[[1]])
      plot_cols = paste0(plot_param,"[",plot_units,"]")
      long_data = lapply(1:nreps, function(x){
        dplyr::as_tibble(traces_list[[x]]) %>%
          dplyr::select(dplyr::all_of(plot_cols)) %>%
          dplyr::mutate(repl = x) %>%
          dplyr::mutate(iter = 0:niter)
      }) %>%
        dplyr::bind_rows() %>%
        tidyr::pivot_longer(cols = plot_cols)

      traces_ggplot = ggplot2::ggplot(
        long_data,
        ggplot2::aes(
          x = .data$iter,
          y = .data$value,
          color = paste0(.data$name, .data$repl)
        )
      ) +
        ggplot2::geom_line(linewidth = 0.5, alpha = 0.25) +
        ggplot2::guides(color = "none") +
        ggplot2::facet_wrap(ggplot2::vars(.data$name), scales = "free")
      if(log_y)
        traces_ggplot = traces_ggplot + ggplot2::scale_y_log10()
      if(print_plots) print(traces_ggplot)
      if(!is.null(save_dir)){
        ggplot2::ggsave(
          filename = paste0("trace_plot_", plot_param,".png"),
          plot = traces_ggplot,
          path = save_dir,
          width = width,
          height = height
        )
      }
    }
  }
}
