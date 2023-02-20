new_EL_list = function(fits, ull, se){
  stopifnot(is.data.frame(fits))
  stopifnot(is.data.frame(ull))
  stopifnot(is.data.frame(se))
  out = list(
    fits = dplyr::as_tibble(fits),
    ull = dplyr::as_tibble(ull),
    se = dplyr::as_tibble(se)
  )
  structure(out, class = "EL_list")
}
