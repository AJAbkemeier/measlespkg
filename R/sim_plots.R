#' Title
#'
#' @param true_model
#' @param sim_model
#' @param n_sims
#' @param print_plots
#' @param save_dir
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
sim_plots = function(true_model, sim_model, n_sims = 100, print_plots = TRUE,
save_dir = NULL, width = 16, height = 12){
  make_sim_tibble = function(ppomp_real, ppomp_sim, n_reps){
    make_obs_tibble = function(ppomp, rep_name){
      real_obs = sapply(1:length(ppomp), function(x) obs(ppomp[[x]]))
      colnames(real_obs) = names(ppomp)
      real_obs = real_obs %>%
        as_tibble() %>%
        mutate(time = time(ppomp[[1]]), rep_name = rep_name) %>%
        pivot_longer(cols = names(ppomp),
                     names_to = "unit", values_to = "cases")
      real_obs
    }
    obs_list = vector("list", n_reps + 1)
    obs_list[[1]] = make_obs_tibble(ppomp_real, "real")
    for(i in 1:n_reps){
      obs_list[[i+1]] = make_obs_tibble(simulate(ppomp_sim), paste0("sim",i))
    }
    bind_rows(obs_list)
  }

  sim_tibble = make_sim_tibble(true_model, sim_model, n_sims)

  bound_tibble = sim_tibble %>%
    filter(rep_name != "real") %>%
    group_by(unit, time) %>%
    summarize(lq = quantile(cases, 0.025, na.rm = TRUE),
              uq = quantile(cases, 0.975, na.rm = TRUE))

  sim_tibble = sim_tibble %>%
    left_join(bound_tibble, by = c("time","unit"))

  first_10_sim_tibble = filter(sim_tibble, rep_name %in% paste0("sim", 1:10))

  ggplot_list = vector("list", 4)
  # Individual sims plots
  ggplot(first_10_sim_tibble, aes(x = time, y = cases, color = rep_name)) +
    geom_line() +
    facet_wrap(vars(unit), scales = "free") +
    geom_line(filter(sim_tibble, rep_name == "real"),
              mapping = aes(x = time, y = cases), color = "black") +
    theme(legend.position = "none") -> ggplot_list[[1]]

  # Individual sims with log scale
  ggplot(first_10_sim_tibble, aes(x = time, y = cases + 1, color = rep_name)) +
    geom_line() +
    facet_wrap(vars(unit), scales = "free") +
    geom_line(filter(sim_tibble, rep_name == "real"),
              mapping = aes(x = time, y = cases + 1), color = "black") +
    theme(legend.position = "none") +
    scale_y_log10() -> ggplot_list[[2]]

  # Shaded 95% confidence intervals
  ggplot(sim_tibble) +
    geom_ribbon(aes(x = time, ymin = lq, ymax = uq),
                color = "red", fill = "pink") +
    facet_wrap(vars(unit), scales = "free") +
    geom_line(filter(sim_tibble, rep_name == "real"),
              mapping = aes(x = time, y = cases), color = "black") +
    theme(legend.position = "none") -> ggplot_list[[3]]

  # Shaded 95% confidence intervals with log scale
  ggplot(sim_tibble) +
    geom_ribbon(aes(x = time, ymin = lq + 1, ymax = uq + 1),
                color = "red", fill = "pink") +
    facet_wrap(vars(unit), scales = "free") +
    geom_line(filter(sim_tibble, rep_name == "real"),
              mapping = aes(x = time, y = cases + 1), color = "black") +
    theme(legend.position = "none") +
    scale_y_log10() -> ggplot_list[[4]]
  if(print_plots){
    for(gp in ggplot_list){
      print(gp)
    }
  }
  if(!is.null(save_dir)){
    dir.create(save_dir)
    for(i in seq_along(ggplot_list)){
      ggsave(filename = paste0("sim_plot",i,".png"), plot = ggplot_list[[i]],
             path = save_dir, width = width, height = height)
    }
  }
}
