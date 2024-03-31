make_toyPomp = function(
    model,
    data_list,
    starting_pparams = NULL
){
  rproc = model$rproc
  dmeas = model$dmeas
  rmeas = model$rmeas
  rinit = model$rinit
  pt = model$pt
  paramnames = model$paramnames
  states = model$states

  units = paste0("unit", 1:length(data_list))

  ## ----pomp-construction-----------------------------------------------
  pomp_list = lapply(seq_along(units), function(i){
    data_list[[i]] |>
      pomp::pomp(
        t0 = with(data_list[[i]], 2*time[1] - time[2]),
        times = "time",
        rprocess = pomp::euler(rproc, delta.t = 1),
        rinit = rinit,
        dmeasure = dmeas,
        rmeasure = rmeas,
        partrans = pt,
        statenames = states,
        paramnames = paramnames
      )
  })
  names(pomp_list) = units

  ## ----panelPomp-construction-----------------------------------------------
  if(is.null(starting_pparams)){
    shared = if(is.null(model$shared_params)){
      NULL
    } else {
      as.numeric(rep(NA, length(model$shared_params)))
    }
    specific = if(is.null(model$specific_params)){
      NULL
    } else {
      matrix(
        NA,
        nrow = length(model$specific_params),
        ncol = length(units)
      )
    }
    class(specific) = "numeric"
    storage.mode(specific) = "numeric"
    rownames(specific) = model$specific_params
    colnames(specific) = units
    names(shared) = model$shared_params
  } else {
    shared = starting_pparams$shared
    specific = as.matrix(starting_pparams$specific)
    if(!setequal(names(shared), model$shared_params)){
      stop(
        "Starting shared parameters do not match parameters in model mechanics.",
        call. = FALSE
      )
    }
    if(!setequal(rownames(specific), model$specific_params)){
      stop(
        "Starting unit-specific parameters do not match parameters in model mechanics.",
        call. = FALSE
      )
    }
  }
  panelPomp::panelPomp(
    pomp_list,
    shared = shared,
    specific = specific
  )
}
