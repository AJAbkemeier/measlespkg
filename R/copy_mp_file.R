#' Copy file from measlespkg misc directory
#'
#' @param file_name String specifying name of file to copy
#' @param to_path String specifying where to copy the file to
#'
#' @return Returns NULL.
#' @export
#'
copy_mp_file = function(
    file_name = "generalPanelFit.R",
    to_path = "."
){
  lib_path = .libPaths()[[1]]
  file_path = paste0(lib_path,"/measlespkg/misc/",file_name)
  system(paste0("cp ",file_path," ",to_path))
  invisible(NULL)
}
