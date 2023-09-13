#' Plot traces from fitted objects
#'
#' @param fitr_list List of `mif2d.ppomp` or `ibpfd_spatPomp` objects.
#' @param plotted_params String specifying which traces should be plotted. This
#'   includes log-likelihoods and shared parameters. Set to `".ALL"` to plot all
#'   traces.
#' @param log_y Boolean specifying whether y-axis should be scaled by log base
#'   10 for positive-valued traces.
#' @param skip_n Skip plotting the first `skip_n` iterations. Useful when
#'   extreme early values make it difficult to observe later behavior.
#' @param lq Lower quantile cutoff. When shading the traces with the log
#'   likelihood of the fit, values below the lower quantile are rounded up to
#'   the lower quantile.
#'
#' @return Returns `NULL`.
#' @export
#'
plot_traces2 = function(
    fitr_list,
    plotted_params = ".ALL",
    log_y = FALSE,
    skip_n = 0,
    lq = 0
){
  stopifnot(inherits(fitr_list[[1]], what = "mif2d.ppomp"))
  unit_names = names(fitr_list[[1]]@unit.objects)
  specific_params = union(rownames(fitr_list[[1]]@specific), "unitLoglik")
  shared_params = union(rownames(fitr_list[[1]]@shared), "loglik")
  traces_tbl = tidy_traces(fitr_list)
  if(!plotted_params == ".ALL"){
    specific_params = intersect(specific_params, plotted_params)
    shared_params = intersect(shared_params, plotted_params)
  }
  for(param in specific_params){
    lapply(unit_names, function(u){
      param_tbl = traces_tbl |>
        dplyr::filter(
          .data$name == param, .data$unit == u, .data$iteration >= skip_n
        ) |>
        dplyr::left_join(
          dplyr::filter(traces_tbl, .data$name == "unitLoglik") |>
            dplyr::select(-"name") |>
            dplyr::rename(ull = .data$value),
          by = dplyr::join_by("rep", "iteration", "unit")
        ) |>
        dplyr::mutate(
          ull = ifelse(
            .data$ull > quantile(.data$ull, lq, na.rm = TRUE),
            .data$ull,
            quantile(.data$ull, lq, na.rm = TRUE)
          )
        )
      gg = ggplot2::ggplot(
        param_tbl,
        ggplot2::aes(
          x = .data$iteration,
          y = .data$value,
          group = .data$rep,
          color = .data$ull
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(ggplot2::vars(.data$unit)) +
        ggplot2::ylab(param)
      if(log_y && all(param_tbl$value > 0)) gg = gg + ggplot2::scale_y_log10()
      gg
    }) |>
      gridExtra::arrangeGrob(grobs = _) |>
      gridExtra::grid.arrange()
  }
  for(param in shared_params){
    param_tbl = traces_tbl |>
      dplyr::filter(.data$name == param, .data$iteration >= skip_n) |>
      dplyr::left_join(
        dplyr::filter(traces_tbl, .data$name == "loglik") |>
          dplyr::select(-"name") |>
          dplyr::rename(ll = .data$value),
        by = dplyr::join_by("rep", "iteration")
      ) |>
      dplyr::mutate(
        ull = ifelse(
          .data$ll > quantile(.data$ll, lq, na.rm = TRUE),
          .data$ll,
          quantile(.data$ll, lq, na.rm = TRUE)
        )
      )
    gg = ggplot2::ggplot(
      param_tbl,
      ggplot2::aes(
        x = .data$iteration,
        y = .data$value,
        group = .data$rep,
        color = .data$ll
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::ylab(param)
    if(log_y && all(param_tbl$value > 0)) gg = gg + ggplot2::scale_y_log10()
    print(gg)
  }
}
