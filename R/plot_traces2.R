#' Plot traces from fitted objects
#'
#' @param fitr_list List of `mif2d.ppomp` or `ibpfd_spatPomp` objects.
#' @param plotted_params String specifying which traces should be plotted. This
#'   includes log-likelihoods and shared parameters. Set to `".ALL"` to plot all
#'   traces.
#' @param log_y Character vector specifying for which parameters traces the
#'   y-axis should be log scaled.
#' @param logit_y Character vector specifying for which parameters traces the
#'   y-axis should be logit scaled.
#' @param skip_n Skip plotting the first `skip_n` iterations. Useful when
#'   extreme early values make it difficult to observe later behavior.
#' @param lq Lower quantile cutoff. When shading the traces with the log
#'   likelihood of the fit, values below the lower quantile are rounded up to
#'   the lower quantile.
#' @param pseudo_shared_param Name of a pseudo-shared parameter in the model
#'   without the number attached at the end. Supplying this argument will cause
#'   the function to to treat the pseudo-shared parameter as unit-specific. Does
#'   nothing if set to NULL.
#'
#' @return Returns `NULL`.
#' @export
#'
plot_traces2 = function(
    fitr_list,
    plotted_params = ".ALL",
    log_y = NULL,
    logit_y = NULL,
    skip_n = 0,
    lq = 0,
    pseudo_shared_param = NULL
){
  stopifnot(inherits(fitr_list[[1]], what = "mif2d.ppomp"))

  unit_names = names(fitr_list[[1]]@unit.objects)

  specific_params = union(
    rownames(fitr_list[[1]]@specific), c("unitLoglik", pseudo_shared_param)
  )
  shared_params =
    setdiff(
      union(names(fitr_list[[1]]@shared), "loglik"),
      paste0(pseudo_shared_param, seq_along(unit_names))
    )
  if(plotted_params != ".ALL"){
    specific_params = intersect(specific_params, plotted_params)
    shared_params = intersect(shared_params, plotted_params)
  }

  traces_tbl = tidy_traces(fitr_list)
  if(!is.null(pseudo_shared_param)){
    stopifnot(length(pseudo_shared_param) == 1)
    psp_regex = paste0("^",pseudo_shared_param,"[1-9][0-9]*$")
    psp_rows = grepl(psp_regex, traces_tbl$name)
    psp_tbl = traces_tbl[psp_rows,] |>
      dplyr::mutate(
        name = as.numeric(gsub(pseudo_shared_param, "", .data$name))
      ) |>
      dplyr::mutate(unit = unit_names[.data$name]) |>
      dplyr::mutate(name = pseudo_shared_param)
    traces_tbl = traces_tbl |>
      dplyr::filter(!psp_rows) |>
      dplyr::bind_rows(psp_tbl)
  }

  ### Unit-specific traces
  ull_tbl = dplyr::filter(traces_tbl, .data$name == "unitLoglik") |>
    dplyr::select(-"name") |>
    dplyr::rename(ull = .data$value) |>
    dplyr::group_by(.data$unit) |>
    dplyr::mutate(
      ull = ifelse(
        .data$ull > stats::quantile(.data$ull, lq, na.rm = TRUE),
        .data$ull,
        stats::quantile(.data$ull, lq, na.rm = TRUE)
      )
    )
  for(param in specific_params){
    lapply(unit_names, function(u){
      param_tbl = traces_tbl |>
        dplyr::filter(
          .data$name == param, .data$unit == u, .data$iteration >= skip_n
        ) |>
        dplyr::left_join(
          ull_tbl, by = dplyr::join_by("rep", "iteration", "unit")
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
      if(param %in% log_y){
        gg = gg + ggplot2::scale_y_continuous(trans = "log")
      } else if(param %in% logit_y){
        gg = gg + ggplot2::scale_y_continuous(trans = "logit")
      }
      gg
    }) |>
      gridExtra::arrangeGrob(grobs = _) |>
      gridExtra::grid.arrange()
  }

  ### Shared traces
  ll_tbl = dplyr::filter(traces_tbl, .data$name == "loglik") |>
    dplyr::select(-"name") |>
    dplyr::rename(ll = .data$value) |>
    dplyr::group_by(.data$unit) |>
    dplyr::mutate(
      ll = ifelse(
        .data$ll > stats::quantile(.data$ll, lq, na.rm = TRUE),
        .data$ll,
        stats::quantile(.data$ll, lq, na.rm = TRUE)
      )
    )
  for(param in shared_params){
    param_tbl = traces_tbl |>
      dplyr::filter(.data$name == param, .data$iteration >= skip_n) |>
      dplyr::left_join(ll_tbl, by = dplyr::join_by("rep", "iteration"))
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
    if(param %in% log_y){
      gg = gg + ggplot2::scale_y_continuous(trans = "log")
    } else if(param %in% logit_y){
      gg = gg + ggplot2::scale_y_continuous(trans = "logit")
    }
    print(gg)
  }
}
