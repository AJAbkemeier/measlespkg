#' Make a list containing necessary specifications for a panelPomp model
#'
#' @name panel_mechanics
#' @param rproc Csnippet that simulates process step.
#' @param dmeas Csnippet that calculates conditional log likelihood.
#' @param rmeas Csnippet that simulates measurement.
#' @param rinit Csnippet that simulates initial states.
#' @param pt Csnippet specifiying how to transform parameters onto the
#'   estimation scale.
#' @param shared_params Character vector of shared parameter names.
#' @param specific_params Character vector of unit-specific parameter names.
#'
#' @return The arguments in list form with class `panel_mechanics`.
#'
NULL

new_panel_mechanics = function(
    rproc,
    dmeas,
    rmeas,
    rinit,
    pt,
    shared_params,
    specific_params
){
  out = list(
    rproc = rproc,
    dmeas = dmeas,
    rmeas = rmeas,
    rinit = rinit,
    pt = pt,
    shared_params = shared_params,
    specific_params = specific_params,
    paramnames = c(shared_params, specific_params)
  )
  structure(out, class = "panel_mechanics")
}

validate_panel_mechanics = function(x){
  if(
    !all(c("rproc", "dmeas", "rmeas", "rinit", "pt", "paramnames") %in% names(x))
  ){
    stop(
      "`x` must have names 'rproc', 'dmeas', 'rmeas', 'rinit', 'pt',
      'shared_params', 'specific_params', and 'paramnames'",
      call. = FALSE
    )
  }
  combined_params = c(x$shared_params, x$specific_params)
  if(!setequal(combined_params, x$paramnames)){
    stop(
      "The union of shared_params and specific_params is not paramnames.",
      call. = FALSE
    )
  }
  if(any(x$specific_params %in% x$shared_params)){
    stop(
      "The intersection of specific_params and shared_params is not empty.",
      call. = FALSE
    )
  }
  x
}

#' @rdname panel_mechanics
#' @export
panel_mechanics = function(
    rproc,
    dmeas,
    rmeas,
    rinit,
    pt,
    shared_params,
    specific_params
){
  x = new_panel_mechanics(
    rproc,
    dmeas,
    rmeas,
    rinit,
    pt,
    shared_params,
    specific_params
  )
  validate_panel_mechanics(x)
}
