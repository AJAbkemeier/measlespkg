cdl_check = function(pomp_list){
  tf = unitlogLik(pomp_list) == -Inf
  if(sum(tf) == 0){
    return(NULL)
  }
  bad_apples = pomp_list[tf]
  bad_indices_list = vector("list", length(bad_apples))
  for(i in seq_along(bad_indices_list)){
    bad_indices_list[[i]] = which(cond.logLik(bad_apples[[i]]) == -Inf)
  }
  names(bad_indices_list) = names(bad_apples)
  bad_indices_list
}