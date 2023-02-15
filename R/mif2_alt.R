#' Title
#'
#' @param measles_ppomp_mod
#' @param param_guess_list
#' @param rs0
#' @param run_level
#' @param nmif
#' @param n_alt
#' @param nreps_mif2
#' @param np_mif2
#' @param cooling_frac
#' @param block
#' @param shared_first
#' @param seed
#' @param write_mif2_to
#'
#' @return
#' @export
#'
#' @examples
mif2_alt = function(measles_ppomp_mod, param_guess_list, rs0, run_level, nmif,
n_alt, nreps_mif2, np_mif2, cooling_frac, block = FALSE, shared_first = TRUE,
seed = NULL, write_mif2_to){
  make_rw_sd = function(rs){
    ivi = grep("_0", x = names(rs))
    # TODO make this capable of using different sd for each ivp
    ivp_rw_sd = lapply(names(rs[ivi]), function(x)
      expression(ivp(rs[ivi][[1]]))
    )
    names(ivp_rw_sd) = names(rs[ivi])
    reg_rw_sd = as.list(rs[-ivi])
    do.call(rw.sd, c(reg_rw_sd, ivp_rw_sd))
  }
  bake(file = write_mif2_to, seed = seed,
       dependson = list(measles_ppomp_mod, run_level),{
    num_of_alternations = nmif/n_alt
    mif2_alt_list = vector("list", num_of_alternations)
    # Shared parameter rw.sd
    specific_str = rownames(pparams(measles_ppomp_mod)[[2]])
    rs_sh = rs0
    rs_sh[specific_str] = 0
    # Specific parameter rw.sd
    rs_sp = rs0 - rs_sh
    #Start by perturbing shared (specific) parameters
    if(shared_first){
      rs1 = rs_sh
      rs2 = rs_sp
    } else {
      rs1 = rs_sp
      rs2 = rs_sh
    }
    rs = rs1
    foreach(i=1:nreps_mif2, .packages = "panelPomp", .combine = c) %dopar% {
      r_shared_params = param_guess_list[["shared"]][[i]]
      r_specific_params = param_guess_list[["specific"]][[i]]
      rw_sd = make_rw_sd(rs)
      mif2(
        measles_ppomp_mod,
        Np = np_mif2,
        cooling.fraction.50 = cooling_frac,
        rw.sd = rw_sd,
        cooling.type = "geometric",
        Nmif = n_alt,
        shared.start = r_shared_params,
        specific.start = r_specific_params,
        block = block
      )
    } -> mif2_alt_list[[1]]
    if(num_of_alternations > 1){
      for(j in 2:num_of_alternations){
        j2 = floor(j/2)+(j %% 2)
        cooling = cooling_frac^(((j2-1)*n_alt)/50)
        if(j %% 2 == 0){
          rs = rs1
        } else {
          rs = rs2
        }
        foreach(i=1:nreps_mif2, .packages = "panelPomp", .combine = c) %dopar% {
          rw_sd = make_rw_sd(rs*cooling)
          mif2(
            mif2_alt_list[[j-1]][[i]],
            Np = np_mif2,
            cooling.fraction.50 = cooling_frac,
            rw.sd = rw_sd,
            cooling.type = "geometric",
            Nmif = n_alt,
            block = block
          )
        } -> mif2_alt_list[[j]]
      }
    }
    mif2_alt_list
  })
}
