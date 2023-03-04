# mif2_out should be a list of mif2 objects
# EL_out should be a list of EL_list objects
new_fit_results = function(mif2_out, EL_out){
  stopifnot(is.list(mif2_out))
  for(ell in EL_out){
    validate_EL_list(ell)
  }
  out = list(
    mif2_out = mif2_out,
    EL_out = EL_out
  )
  structure(out, class = "fit_results")
}
