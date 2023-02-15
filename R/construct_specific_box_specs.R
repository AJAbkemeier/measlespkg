#' Title
#'
#' @param specific_pparams
#' @param radii_tbl
#'
#' @return
#' @export
#'
#' @examples
construct_specific_box_specs = function(specific_pparams, radii_tbl){
  specific_centers_tbl = bind_cols(param = rownames(specific_pparams),
                                   specific_pparams)
  specific_box =  radii_tbl %>%
    right_join(specific_centers_tbl, by = "param") %>%
    pivot_longer(3:length(.), values_to = "center", names_to = "unit") %>%
    arrange(unit)
  specific_box
}
