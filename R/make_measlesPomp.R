#' Skeleton for measles_ppomp functions
#'
#' @param data
#' @param starting_pparams
#' @param model
#' @param AK_interp
#' @param sim_obs_list
#'
#' @return
#' @export
#'
#' @examples
#' make_measlesPomp(twentycities, AK_pparams, model_mechanics_001())
make_measlesPomp = function(
    data,
    starting_pparams,
    model,
    AK_interp = TRUE,
    sim_obs_list = NULL
){
  rproc = model$rproc
  dmeas = model$dmeas
  rmeas = model$rmeas
  rinit = model$rinit
  pt = model$pt
  paramnames = model$paramnames
  measles = data$measles
  demog = data$demog
  coord = data$coord

  ## ----prep-data-------------------------------------------------
  units = unique(measles$unit)
  # Obs list
  dat_list = vector("list", length(units))
  # Population list
  demog_list = vector("list", length(units))
  for(i in seq_along(units)){
    dat_list[[i]] = measles %>%
      dplyr::mutate(year = as.integer(format(date,"%Y"))) %>%
      dplyr::filter(
        .data$unit == units[[i]] & .data$year >= 1950 & .data$year < 1964
      ) %>%
      dplyr::mutate(
        time = julian(.data$date, origin = as.Date("1950-01-01"))/365.25 + 1950
      ) %>%
      dplyr::filter(.data$time > 1950 & .data$time < 1964) %>%
      dplyr::select(.data$time, .data$cases)
    if(!is.null(sim_obs_list)) dat_list[[i]]$cases = sim_obs_list[[i]]

    demog_list[[i]] = demog %>%
      dplyr::filter(.data$unit == units[[i]]) %>%
      dplyr::select(-.data$unit)
  }
  ## ----prep-covariates-------------------------------------------------
  covar_list = vector("list", length(units))
  for(i in seq_along(units)){
    dmgi = demog_list[[i]]
    times = seq(from = min(dmgi$year), to = max(dmgi$year), by = 1/12)
    if(AK_interp){
      pop_interp = stats::predict(
        stats::smooth.spline(x = dmgi$year, y = dmgi$pop),
        x = times
      )$y
      births_interp = stats::predict(
        stats::smooth.spline(x = dmgi$year + 0.5, y = dmgi$births),
        x = times - 4
      )$y
    } else {
      pop_interp = stats::approx(
        x = dmgi$year,
        y = dmgi$pop,
        xout = times
      )$y
      births_interp = stats::approx(
        x = dmgi$year,
        y = dmgi$births,
        xout = times - 4
      )$y
    }
    covar_list[[i]] = dmgi %>%
    dplyr::summarize(
        time = times,
        pop = pop_interp,
        birthrate = births_interp
    )
    covar_list[[i]] = covar_list[[i]] %>%
    dplyr::mutate(
      pop_1950 = dplyr::filter(covar_list[[i]],covar_list[[i]]$time == 1950)$pop
    )
  }
  for(i in seq_along(units)){
    log_pop_1950 = sapply(seq_along(units), function(x)
      log(covar_list[[x]][["pop_1950"]][[1]])
    )
    covar_list[[i]] = covar_list[[i]] %>%
      dplyr::mutate(
        std_log_pop_1950 = (log(.data$pop_1950) - mean(log_pop_1950))/
          stats::sd(log_pop_1950)
      )
  }

  ## ----pomp-construction-----------------------------------------------
  lapply(seq_along(units), function(i){
    time = covar_list[[i]][[1]]
    dat_list[[i]] %>%
      pomp::pomp(
        t0 = with(dat_list[[i]], 2*time[1] - time[2]),
        time = "time",
        rprocess = pomp::euler(rproc, delta.t = 1/365.25),
        rinit = rinit,
        dmeasure = dmeas,
        rmeasure = rmeas,
        covar = pomp::covariate_table(covar_list[[i]], times = "time"),
        accumvars = c("C","W"),
        partrans = pt,
        statenames = c("S","E","I","R","C","W"),
        paramnames = paramnames
      )
  }) -> pomp_list
  names(pomp_list) = units

  ## ----panelPomp-construction-----------------------------------------------
  panelPomp::panelPomp(
    pomp_list,
    shared = starting_pparams$shared,
    specific = starting_pparams$specific
  )
}
