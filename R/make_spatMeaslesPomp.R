#' Make a `spatPomp` model using measles data
#'
#' @param data List in the format of `twentycities`.
#' @param model List of objects in the format of the output for the
#'   `model_mechanics_XXX()` functions.
#' @param interp_method Method used to interpolate population and births.
#'   Possible options are `"shifted_splines"` and `"linear"`.
#' @param dt Size of the time step.
#'
#' @return A `spatPomp` object using the data and model supplied.
#' @export
#'
make_spatMeaslesPomp = function(
    data,
    model,
    interp_method = c("shifted_splines", "linear"),
    dt = 1/365.25,
    ...
){
  units = unique(data$measles$unit)
  U = length(units)
  cases_df = data$measles |>
    dplyr::mutate(year = as.integer(format(date,"%Y"))) |>
    dplyr::filter(.data$year >= 1950 & .data$year < 1964) |>
    dplyr::mutate(
      time = julian(.data$date, origin = as.Date("1950-01-01"))/365.25 + 1950
    ) |>
    dplyr::filter(.data$time > 1950 & .data$time < 1964) |>
    dplyr::select(.data$unit, .data$time, .data$cases) |>
    dplyr::arrange(.data$time, .data$unit)

  covar_list = vector("list", length(units))
  for(i in seq_along(units)){
    dmgi = dplyr::filter(data$demog, .data$unit == units[[i]])
    times = seq(from = min(dmgi$year), to = max(dmgi$year), by = 1/12)
    #times = dplyr::filter(cases_df, unit == units[[1]])$time
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
    covar_list[[i]] = dmgi |>
      dplyr::reframe(
        time = times,
        pop = pop_interp,
        birthrate = births_interp
      )
    covar_list[[i]] = covar_list[[i]] |>
      dplyr::mutate(
        pop_1950 = dplyr::filter(
          data$demog, unit == units[[i]], year == 1950
        )$pop,
        unit = units[[i]]
      )
  }
  for(i in seq_along(units)){
    log_pop_1950 = sapply(seq_along(units), function(x)
      log(covar_list[[x]][["pop_1950"]][[1]])
    )
    covar_list[[i]] = covar_list[[i]] |>
      dplyr::mutate(
        std_log_pop_1950 = (log(.data$pop_1950) - mean(log_pop_1950))/
          stats::sd(log_pop_1950)
      )
  }
  covar_df = dplyr::bind_rows(covar_list) |>
    dplyr::arrange(.data$time, .data$unit) |>
    dplyr::select(unit, time, dplyr::everything())

  # Haversine formula for great circle distance between two points on a sphere
  # of radius r. Here, r defaults to a mean radius for the earth, in miles.
  distGreatCircle <- function(p1, p2, r = 3963.191) {
    Lon1 <- p1[,1]*pi/180
    Lat1 <- p1[,2]*pi/180
    Lon2 <- p2[,1]*pi/180
    Lat2 <- p2[,2]*pi/180
    a <- sin((Lat2-Lat1)/2)^2 + cos(Lat1)*cos(Lat2)*sin((Lon2-Lon1)/2)^2
    atan2(sqrt(a), sqrt(1 - a))*2*r
  }

  long_lat <- data$coord[,c("long","lat")]
  dmat <- matrix(0, U, U)
  for(u1 in 1:U) {
    for(u2 in 1:U) {
      dmat[u1,u2] = round(distGreatCircle(long_lat[u1,], long_lat[u2,]), 1)
    }
  }

  p <- data$demog |>
    dplyr::group_by(.data$unit) |>
    dplyr::summarize(mean_pop = mean(.data$pop)) |>
    tibble::deframe()
  v_by_g <- matrix(0, U, U)
  dist_mean <- sum(dmat)/(U*(U - 1))
  p_mean <- mean(p)
  for(u1 in 2:U){
    for(u2 in 1:(u1-1)){
      v_by_g[u1,u2] <- (dist_mean*p[u1]*p[u2]) / (dmat[u1,u2] * p_mean^2)
      v_by_g[u2,u1] <- v_by_g[u1,u2]
    }
  }
  to_C_array <- function(v) paste0("{", paste0(v, collapse = ","), "}")
  v_by_g_C_rows <- apply(v_by_g, 1, to_C_array)
  v_by_g_C_array <- to_C_array(v_by_g_C_rows)
  v_by_g_C <- pomp::Csnippet(
    paste0("const double v_by_g[",U,"][",U,"] = ",v_by_g_C_array,"; ")
  )

  set_unit_specific = pomp::Csnippet(
    paste0("const int ", model$spp_names,"_unit = 1;\n", collapse=" ")
  )
  set_shared = pomp::Csnippet(
    paste0("const int ", model$shp_names,"_unit = 0;\n", collapse=" ")
  )

  measles_globals = pomp::Csnippet(
    paste(v_by_g_C, set_unit_specific, set_shared, sep = "\n ")
  )

  # add a "1" for shared parameter names to make the pointers work
  # measles_paramnames = unlist(c(
  #   lapply(model$spp_names, function(x,U) paste0(x,1:U),U),
  #   lapply(model$shp_names, function(x) paste0(x,1))
  # ))

  first_unit_df = dplyr::filter(cases_df, unit == cases_df$unit[[1]])
  model_out = spatPomp::spatPomp(
    cases_df,
    units = "unit",
    times = "time",
    t0 = with(first_unit_df, 2*time[1] - time[2]),
    unit_statenames = c('S','E','I','C'),
    covar = covar_df,
    rprocess = pomp::euler(model$rproc, delta.t = dt),
    unit_accumvars = 'C',
    paramnames = c(model$total_shp_names, model$total_spp_names),
    partrans = model$pt,
    globals = measles_globals,
    rinit = model$rinit,
    dmeasure = model$dmeas,
    rmeasure = model$rmeas,
    dunit_measure = model$dunit_measure
  )
  total_param_names = c(model$total_shp_names, model$total_spp_names)
  dummy_params = rep(0, length(total_param_names))
  names(dummy_params) = total_param_names
  pomp::coef(model_out) = dummy_params
  model_out
}
