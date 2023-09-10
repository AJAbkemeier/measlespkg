#' Make a list containing necessary specifications for a spatPomp model
#'
#' @name spat_mechanics
#' @param rproc spatPomp_Csnippet that simulates process step.
#' @param dmeas spatPomp_Csnippet that calculates conditional log likelihood.
#' @param dunit_measure spatPomp_Csnippet that calculates conditional log
#'   likelihood for a single unit.
#' @param rmeas spatPomp_Csnippet that simulates measurement.
#' @param rinit spatPomp_Csnippet that simulates initial states.
#' @param pt Csnippet specifiying how to transform parameters onto the
#'   estimation scale.
#' @param shared_params Character vector of shared parameter names.
#' @param specific_params Character vector of unit-specific parameter names.
#' @param U Number of units.
#'
#' @return The arguments in list form with class `spat_mechanics`.
#'
NULL

new_spat_mechanics = function(
    rproc,
    dmeas,
    dunit_measure,
    rmeas,
    rinit,
    pt,
    shared_params,
    specific_params,
    expanded_shared_params,
    expanded_specific_params
){
  out = list(
    rproc = rproc,
    dmeas = dmeas,
    dunit_measure = dunit_measure,
    rmeas = rmeas,
    rinit = rinit,
    pt = pt,
    shared_params = shared_params,
    specific_params = specific_params,
    paramnames = c(shared_params, specific_params),
    expanded_shared_params = expanded_shared_params,
    expanded_specific_params = expanded_specific_params
  )
  structure(out, class = "spat_mechanics")
}

validate_spat_mechanics = function(x){
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

#' @rdname spat_mechanics
#' @export
spat_mechanics = function(
    rproc,
    dmeas,
    dunit_measure,
    rmeas,
    rinit,
    pt,
    shared_params,
    specific_params,
    U
){
  expanded_shared_params =
    unlist(lapply(shared_params, function(x,U) paste0(x,1:U),U))
  expanded_specific_params =
    unlist(lapply(specific_params, function(x,U) paste0(x,1:U),U))
  x = new_spat_mechanics(
    rproc = rproc,
    dmeas = dmeas,
    dunit_measure = dunit_measure,
    rmeas = rmeas,
    rinit = rinit,
    pt = pt,
    shared_params = shared_params,
    specific_params = specific_params,
    expanded_shared_params = expanded_shared_params,
    expanded_specific_params = expanded_specific_params
  )
  validate_spat_mechanics(x)
}
