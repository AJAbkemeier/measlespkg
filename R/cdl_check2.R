cdl_check2 = function(mif2_list, threshold = 1e-18){
  sapply(seq_along(mif2_list), 
    function(y) sapply(1:20,
      function(x) sum(cond.logLik(mif2_list[[y]][[x]]) == log(threshold))
    )
  )
}