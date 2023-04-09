#' Gather information on fit results from subdirectories and tidy them
#'
#' @param file_names Character vector of names of rds files to load.
#' @param parent_dir Path to the directory that we want to search the
#' subdirectories of.
#'
#' @return `data.frame` where each row contains information for a unit from a
#' `mif2` replication. Columns include information such as the path to the rds
#' file, the number of particles used for `mif2` and likelihood evaluation, and
#' the parameters that the likelihood was evaluated at.
#' @export
#'
gather_results = function(file_names, parent_dir = "."){
  out_paths = c()
  for(fn in file_names){
    out_paths = c(
      out_paths,
      list.files(
        path = parent_dir,
        pattern = paste0("*", fn),
        recursive = TRUE
      )
    )
  }
  out_paths = paste0(parent_dir,"/",out_paths)
  lapply(out_paths, function(x){
    loaded_obj = readRDS(x)
    if(is(loaded_obj) == "fit_results"){
      ELL = loaded_obj$EL_out[[1]]
      MIF2O = loaded_obj$mif2_out[[1]]
      Nmif = MIF2O@Nmif
      cf = MIF2O@cooling.fraction.50
      block = MIF2O@block
      Np = MIF2O@Np
      np_eval = ELL$np_pf
      nreps_eval = ELL$nreps
    }
    if(is(loaded_obj) == "EL_list"){
      ELL = loaded_obj
      Nmif = NA
      cf = NA
      block = NA
      Np = NA
      np_eval = ELL$np_pf
      nreps_eval = ELL$nreps
    }
    ELL |>
      measlespkg::tidy_pfilter_dfs() |>
      dplyr::mutate(
        path = x,
        nmif = Nmif,
        np_mif = Np,
        cooling_frac = cf,
        np_eval = np_eval,
        nreps_eval = nreps_eval,
        block = block
      ) |>
      dplyr::select(
        path, nmif, np_mif, cooling_frac, block, np_eval, nreps_eval,
        dplyr::everything()
      )
  }) |>
    dplyr::bind_rows()
}
