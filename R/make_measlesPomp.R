#' Make a panelPomp model using measles data
#'
#' @param data List in the format of `twentycities`.
#' @param starting_pparams Parameters in the format of `pparams()` output.
#' @param model List of objects in the format of the output for the
#' `model_mechanics_XXX()` functions.
#' @param interp_method Method used to interpolate population and births.
#' Possible options are `"shifted_splines"` and `"linear`.
#' @param custom_obs_list List of observations where each element supplies
#' observations for a different city. Useful when using simulated observations.
#' Set to `NULL` to use real observations.
#'
#' @return A panelPomp object using the data and model supplied.
#' @export
#'
#' @examples
#' make_measlesPomp(twentycities, AK_pparams, model_mechanics_001())
make_measlesPomp = function(
    data,
    starting_pparams,
    model,
    interp_method = c("shifted_splines", "linear"),
    custom_obs_list = NULL
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
    if(!is.null(custom_obs_list)) dat_list[[i]]$cases = custom_obs_list[[i]]

    demog_list[[i]] = demog %>%
      dplyr::filter(.data$unit == units[[i]]) %>%
      dplyr::select(-.data$unit)
  }
  ## ----prep-covariates-------------------------------------------------
  covar_list = vector("list", length(units))
  for(i in seq_along(units)){
    dmgi = demog_list[[i]]
    times = seq(from = min(dmgi$year), to = max(dmgi$year), by = 1/12)
    switch(interp_method[[1]],
      shifted_splines = {
        pop_interp = stats::predict(
          stats::smooth.spline(x = dmgi$year, y = dmgi$pop),
          x = times
        )$y
        births_interp = stats::predict(
          stats::smooth.spline(x = dmgi$year + 0.5, y = dmgi$births),
          x = times - 4
        )$y
      },
      linear = {
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
    )
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
    specific = as.matrix(starting_pparams$specific)
  )
}
