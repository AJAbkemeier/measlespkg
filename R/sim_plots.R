#' Plot simulated observations against real observations
#'
#' @param true_model `panelPomp` object to get true observations from.
#' @param sim_model `panelPomp` object to simulate observations from.
#' @param n_sims Number of simulations to generate.
#' @param print_plots Boolean specifying whether plots should be printed.
#' @param save_dir String specifying where plots should be saved. No plots are
#' saved if `NULL`.
#' @param width Width of plots.
#' @param height Height of plots.
#'
#' @return Returns `NULL`.
#' @export
#'
#' @examples
#' mod = AK_model()
#' sim_plots(mod, mod, n_sims = 10)
sim_plots = function(
    true_model,
    sim_model,
    n_sims = 100,
    print_plots = TRUE,
    save_dir = NULL,
    width = 16,
    height = 12
  ){
  make_sim_tibble = function(ppomp_real, ppomp_sim, n_reps){
    make_obs_tibble = function(ppomp, rep_name){
      real_obs = sapply(1:length(ppomp), function(x) pomp::obs(ppomp[[x]]))
      colnames(real_obs) = names(ppomp)
      real_obs = real_obs %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(time = pomp::time(ppomp[[1]]), rep_name = rep_name) %>%
        tidyr::pivot_longer(
          cols = names(ppomp),
          names_to = "unit",
          values_to = "cases"
        )
      real_obs
    }
    obs_list = vector("list", n_reps + 1)
    obs_list[[1]] = make_obs_tibble(ppomp_real, "real")
    for(i in 1:n_reps){
      obs_list[[i+1]] = make_obs_tibble(
        panelPomp::simulate(ppomp_sim),
        paste0("sim", i)
      )
    }
    dplyr::bind_rows(obs_list)
  }
  sim_tibble = make_sim_tibble(true_model, sim_model, n_sims)
  bound_tibble = sim_tibble %>%
    dplyr::filter(.data$rep_name != "real") %>%
    dplyr::group_by(.data$unit, .data$time) %>%
    dplyr::summarize(
      lq = stats::quantile(.data$cases, 0.025, na.rm = TRUE),
      uq = stats::quantile(.data$cases, 0.975, na.rm = TRUE)
    )

  sim_tibble = sim_tibble %>%
    dplyr::left_join(bound_tibble, by = c("time","unit"))

  first_10_sim_tibble = sim_tibble %>%
    dplyr::filter(.data$rep_name %in% paste0("sim", 1:10))

  ggplot_list = vector("list", 4)
  # Individual sims plots
  ggplot2::ggplot(
    first_10_sim_tibble,
    ggplot2::aes(x = .data$time, y = .data$cases, color = .data$rep_name)
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(.data$unit), scales = "free") +
    ggplot2::geom_line(
      dplyr::filter(sim_tibble, .data$rep_name == "real"),
      mapping = ggplot2::aes(x = .data$time, y = .data$cases),
      color = "black"
    ) +
    ggplot2::theme(legend.position = "none") -> ggplot_list[[1]]

  # Individual sims with log scale
  ggplot2::ggplot(
    first_10_sim_tibble,
    ggplot2::aes(x = .data$time, y = .data$cases + 1, color = .data$rep_name)
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(.data$unit), scales = "free") +
    ggplot2::geom_line(
      dplyr::filter(sim_tibble, .data$rep_name == "real"),
      mapping = ggplot2::aes(x = .data$time, y = .data$cases + 1),
      color = "black"
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_y_log10() -> ggplot_list[[2]]

  # Shaded 95% confidence intervals
  ggplot2::ggplot(sim_tibble) +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = .data$time, ymin = .data$lq, ymax = .data$uq),
      color = "red",
      fill = "pink"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$unit), scales = "free") +
    ggplot2::geom_line(
      dplyr::filter(sim_tibble, .data$rep_name == "real"),
      mapping = ggplot2::aes(x = .data$time, y = .data$cases),
      color = "black"
    ) +
    ggplot2::theme(legend.position = "none") -> ggplot_list[[3]]

  # Shaded 95% confidence intervals with log scale
  ggplot2::ggplot(sim_tibble) +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = .data$time, ymin = .data$lq + 1, ymax = .data$uq + 1),
      color = "red",
      fill = "pink"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$unit), scales = "free") +
    ggplot2::geom_line(
      dplyr::filter(sim_tibble, .data$rep_name == "real"),
      mapping = ggplot2::aes(x = .data$time, y = .data$cases + 1),
      color = "black"
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_y_log10() -> ggplot_list[[4]]
  if(print_plots){
    for(gp in ggplot_list){
      print(gp)
    }
  }
  if(!is.null(save_dir)){
    dir.create(save_dir)
    for(i in seq_along(ggplot_list)){
      ggplot2::ggsave(
        filename = paste0("sim_plot",i,".png"),
        plot = ggplot_list[[i]],
        path = save_dir,
        width = width,
        height = height
      )
    }
  }
}
