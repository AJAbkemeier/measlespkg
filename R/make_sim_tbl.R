#' Make a tibble filled with simulated observations.
#'
#' @param sim_model `panelPomp` or `spatPomp` object to simulate from.
#' @param n_sims Number of simulations to perform.
#' @param true_model Object containing the real observations and times. Set to
#'   `NULL` to exclude.
#'
#' @return A tibble with columns for observation time, replication name, unit,
#'   and cases.
#' @export
#'
#' @examples
#' \dontrun{
#' AK_mod = AK_model()
#' make_sim_tbl(AK_mod, n_sims = 10, true_model = AK_mod)
#' }
make_sim_tbl = function(sim_model, n_sims, true_model = NULL){
  make_obs_tbl = function(model_obj, rep_name){
    observations = obs2(model_obj) |> t()
    if(inherits(model_obj, "panelPomp")){
      time = pomp::time(model_obj[[1]])
      units = names(model_obj)
    } else {
      time = pomp::time(model_obj)
      units = spatPomp::unit_names(model_obj)
    }
    colnames(observations) = units
    observations = observations |>
      dplyr::as_tibble() |>
      dplyr::mutate(time = time, rep_name = rep_name) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(units),
        names_to = "unit",
        values_to = "cases"
      )
    observations
  }
  true_model_included = !is.null(true_model)
  n_total = n_sims + true_model_included
  obs_list = vector("list", n_total)
  if(true_model_included) obs_list[[1]] = make_obs_tbl(true_model, "real")
  for(i in 1:n_sims){
    obs_list[[i + true_model_included]] = make_obs_tbl(
      panelPomp::simulate(sim_model),
      paste0("sim", i)
    )
  }
  dplyr::bind_rows(obs_list)
}
