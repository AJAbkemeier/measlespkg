validate_EL_list = function(x){
  if(!all(c("fits", "ull", "se") %in% names(x))){
    stop(
      "`x` must have names 'fits', 'ull', and 'se'",
      call. = FALSE
    )
  }
  if(!all(is.data.frame(x$fits), is.data.frame(x$ull), is.data.frame(x$se))){
    stop(
      "'fits', 'ull', and 'se' must be of class data.frame"
    )
  }
  if(!all(c("logLik", "se") %in% colnames(x$fits))){
    stop(
      "`fits` must have 'logLik' and 'se' columns",
      call. = FALSE
    )
  }
  x
}
