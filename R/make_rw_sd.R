#' Turn named numeric vector into `rw_sd`
#'
#' @param rw_sd_vec Named numerical vector. Names are parameters, values are
#'   desired random walk standard deviations. Names of initial value parameters
#'   should end in _0.
#' @param weighted_param Name of the weighted parameter in a weighted model. Set
#'   to NULL (the default) if model does not contain a weighted parameter.
#'
#' @return Returns `rw_sd` object.
#' @export
#'
#' @examples
#' rw_sd_vec = c(alpha = 0.02, gamma = 0.01, E_0 = 0.003, I_0 = 0.006)
#' make_rw_sd(rw_sd_vec)
make_rw_sd = function(rw_sd_vec, weighted_param = NULL){
  special = !is.null(weighted_param)
  ivp_indices = grep("_0", x = names(rw_sd_vec))
  if(special){
    special_indices = grep(
      paste0(weighted_param,"[1-9][0-9]*$"),
      x = names(rw_sd_vec)
    )
    si_ordered = sapply(seq_along(special_indices), function(x){
      grep(paste0("^",weighted_param, x,"$"), names(rw_sd_vec))
    })
  } else {
    special_indices = NULL
  }
  if(length(intersect(special_indices, ivp_indices)) > 0){
    stop(
      "IVPs cannot overlap with weighted parameters.",
      call. = FALSE
    )
  }
  reg_indices = setdiff(seq_along(rw_sd_vec), c(special_indices, ivp_indices))
  if(length(ivp_indices) > 0){
    ivp_rw_sd = lapply(names(rw_sd_vec[ivp_indices]), function(x){
      eval(bquote(expression(ivp(rw_sd_vec[[.(x)]]))))
    })
    names(ivp_rw_sd) = names(rw_sd_vec[ivp_indices])
    reg_rw_sd = as.list(rw_sd_vec[reg_indices])
  } else {
    ivp_rw_sd = NULL
    reg_rw_sd = as.list(rw_sd_vec)
  }
  if(special){
    special_rw_sd = as.list(rw_sd_vec[si_ordered])
    out = lapply(seq_along(special_indices), function(x){
      special_rw_sd[-x] = 0
      do.call(pomp::rw_sd, c(reg_rw_sd, ivp_rw_sd, special_rw_sd))
    })
  } else {
    out = do.call(pomp::rw_sd, c(reg_rw_sd, ivp_rw_sd))
  }
  out
}
