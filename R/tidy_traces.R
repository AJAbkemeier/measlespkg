#' Return tidy tibble of traces
#'
#' @param x A list of `mif2d.ppomp` objects.
#'
#' @return A tibble with columns for replication number, unit name, trace name,
#'   and trace value. If the parameter in question is shared, the unit name will
#'   show up as "SHARED".
#' @export
#'
tidy_traces = function(x){
  stopifnot(is.list(x))
  lapply(seq_along(x), function(u){
    panelPomp::traces(x[[u]]) |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "iteration") |>
      tidyr::pivot_longer(-"iteration", names_to = "str") |>
      dplyr::mutate(rep = u, str = sub("]$", "", .data$str)) |>
      tidyr::separate_wider_delim(
        cols = "str",
        delim = "[",
        too_few = "align_start",
        names = c("name", "unit")
      ) |>
      dplyr::mutate(
        unit = ifelse(is.na(.data$unit), "SHARED", .data$unit)
      )
  }) |>
    dplyr::bind_rows() |>
    dplyr::select("rep", "unit", "name", "iteration", "value")
}
