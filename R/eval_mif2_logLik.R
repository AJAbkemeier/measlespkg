# Evaluates log likelihood of model at parameters based on mif2 object, then 
# saves results to file.
#
# mif2_in actually just needs to be a list of panelPomp objects
eval_mif2_logLik = function(mif2_in, write_to, ncores, np_pf, nreps,
                            seed = 998468235, divisor = 8164, 
                            pfilter_obj_write_to = NULL){
  pf_logLik_frame = data.frame(
    logLik = rep(0, length(mif2_in)),
    se = rep(0, length(mif2_in))
  ) %>% cbind(
    rbind(t(sapply(mif2_in, coef)))
  )
  
  pf_unitlogLik_list = vector("list", length(mif2_in))
  pf_unitSE_list = vector("list", length(mif2_in))
  
  for(i in seq_along(mif2_in)){
    registerDoParallel(cores = ncores)
    seed_i = if(is.null(seed) | is.null(divisor)) NULL else (seed*i) %% divisor
    registerDoRNG(seed_i)
    tic()
    foreach(j = 1:nreps, .packages = "panelPomp", .combine=rbind) %dopar% {
      pfilter_obj = pfilter(mif2_in[[i]], Np=np_pf)
      if(!is.null(pfilter_obj_write_to) & i == 1){
        save(pfilter_obj, file = pfilter_obj_write_to)
      }
      unitlogLik(pfilter_obj)
    } -> pf_unitlogLik_matrix
    toc()
    pf_logLik_frame[i,1:2] = 
      panel_logmeanexp(pf_unitlogLik_matrix, MARGIN = 2, se = TRUE)
    unit_calcs = apply(pf_unitlogLik_matrix, MARGIN = 2, 
                       FUN = logmeanexp, se = TRUE)
    rownames(unit_calcs)[[1]] = "loglik"
    pf_unitlogLik_list[[i]] = subset(unit_calcs, 
                                     rownames(unit_calcs) == "loglik") %>%
      as.data.frame()
    pf_unitSE_list[[i]] = subset(unit_calcs, 
                                 rownames(unit_calcs) == "se") %>%
      as.data.frame()
  }
  pf_unitlogLik_frame = data.frame(bind_rows(pf_unitlogLik_list))
  rownames(pf_unitlogLik_frame) = 1:nrow(pf_unitlogLik_frame)
  pf_unitSE_frame = data.frame(bind_rows(pf_unitSE_list))
  rownames(pf_unitSE_frame) = 1:nrow(pf_unitSE_frame)
  print(pf_logLik_frame[,1:2])
  save(pf_logLik_frame, pf_unitlogLik_frame, pf_unitSE_frame, file = write_to)
}
