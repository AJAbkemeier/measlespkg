#' Turn named numeric vector into rw_sd
#'
#' @param rw_sd_vec Named numerical vector. Names are parameters, values are
#' desired random walk standard deviations. Names of initial value parameters
#' should end in _0.
#'
#' @return Returns rw_sd object.
#' @export
#'
#' @examples
#' rw_sd_vec = c(alpha = 0.02, gamma = 0.01, E_0 = 0.003, I_0 = 0.006)
#' make_rw_sd(rw_sd_vec)
make_rw_sd = function(rw_sd_vec){
  ivp_indices = grep("_0", x = names(rw_sd_vec))
  ivp_rw_sd = lapply(names(rw_sd_vec[ivp_indices]), function(x){
    eval(bquote(expression(ivp(rw_sd_vec[[.(x)]]))))
  })
  names(ivp_rw_sd) = names(rw_sd_vec[ivp_indices])
  reg_rw_sd = as.list(rw_sd_vec[-ivp_indices])
  do.call(pomp::rw_sd, c(reg_rw_sd, ivp_rw_sd))
}
