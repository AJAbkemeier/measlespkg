#' Gather information on fit results from subdirectories and tidy them
#'
#' Given a parent directory and a vector of rds file names, this function will
#' attempt to load any rds files with those names under the parent directory. If
#' the rds file contains a `fit_results` object or an `EL_list` object, it will
#' call `tidy_results()` on it and row bind the resultings data frames. If an
#' error occurs at any point when trying to load and tidy a given file, the file
#' will be skipped over.
#'
#' @param file_names Character vector of names of rds files to load.
#' @param parent_dir Path to the directory that we want to search the
#'   subdirectories of.
#'
#' @return `data.frame` where each row contains information for a unit from a
#'   model replication. Columns include information such as the path to the Rds
#'   file, the number of particles used for the fitting algorithm and likelihood
#'   evaluation, and the parameters that the likelihood was evaluated at.
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
  lapply(out_paths, function(path){
    out = try({
      loaded_obj = readRDS(path)
      loaded_obj = if(inherits(loaded_obj, "EL_list")){
        loaded_obj
      } else {
        update_fit_results(loaded_obj)
      }
      tidy_results(loaded_obj, path = path)
    })
    if(inherits(out, "try-error")){
      NULL
    } else {
      out
    }
  }) |>
    dplyr::bind_rows()
}
