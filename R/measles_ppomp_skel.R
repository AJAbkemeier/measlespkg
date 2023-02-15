#' Skeleton for measles_ppomp functions.
#'
#' @param rproc
#' @param dmeas
#' @param rmeas
#' @param rinit
#' @param extra_param_start
#' @param pt
#' @param paramnames
#' @param AK_interp
#' @param shared_str
#' @param sim_model
#' @param alternate_data
#'
#' @return
#' @export
#'
#' @examples
measles_ppomp_skel = function(
    rproc,
    dmeas,
    rmeas,
    rinit,
    extra_param_start,
    pt,
    paramnames,
    AK_interp,
    shared_str = "mu",
    sim_model = NULL,
    alternate_data = NULL){
  ## ----load-data-------------------------------------------------
  if(is.null(alternate_data)){
    # Loaded as "measles"
    load("input/twentycities.rda")

    # data used for He et al 2010, following their decision
    # to remove 3 data points

    # > measles[13769+1:5,]
    #            town       date cases
    # 13770 Liverpool 1955-11-04    10
    # 13771 Liverpool 1955-11-11    25
    # 13772 Liverpool 1955-11-18   116
    # 13773 Liverpool 1955-11-25    17
    # 13774 Liverpool 1955-12-02    18

    measles[13772,"cases"] <- NA

    # > measles[13949+1:5,]
    #            town       date cases
    # 13950 Liverpool 1959-04-17   143
    # 13951 Liverpool 1959-04-24   115
    # 13952 Liverpool 1959-05-01   450
    # 13953 Liverpool 1959-05-08    96
    # 13954 Liverpool 1959-05-15   157

    measles[13952,"cases"] <- NA

    # > measles[19551+1:5,]
    #             town       date cases
    # 19552 Nottingham 1961-08-18     6
    # 19553 Nottingham 1961-08-25     7
    # 19554 Nottingham 1961-09-01    66
    # 19555 Nottingham 1961-09-08     8
    # 19556 Nottingham 1961-09-15     7

    measles[19554,"cases"] <- NA

    ## ----mles-----------------------------------------------
    read_csv("
      town,loglik,loglik.sd,mu,delay,sigma,gamma,rho,R0,amplitude,alpha,iota,cohort,psi,S_0,E_0,I_0,R_0,sigmaSE
      Bedwellty,-1125.1,0.14,0.02,4,57.9,146,0.311,24.7,0.16,0.937,0.0396,0.351,0.951,0.0396,2.64e-05,2.45e-05,0.96,0.0611
      Birmingham,-3239.3,1.55,0.02,4,45.6,32.9,0.544,43.4,0.428,1.01,0.343,0.331,0.178,0.0264,8.96e-05,0.000335,0.973,0.0611
      Bradford,-2586.6,0.68,0.02,4,45.6,129,0.599,32.1,0.236,0.991,0.244,0.297,0.19,0.0365,7.41e-06,4.59e-06,0.964,0.0451
      Bristol,-2681.6,0.5,0.02,4,64.3,82.6,0.626,26.8,0.203,1.01,0.441,0.344,0.201,0.0358,9.62e-06,5.37e-06,0.964,0.0392
      Cardiff,-2364.9,0.73,0.02,4,39,143,0.602,34.4,0.223,0.996,0.141,0.267,0.27,0.0317,1.01e-05,9.21e-06,0.968,0.0539
      Consett,-1362.9,0.73,0.02,4,42.6,172,0.65,35.9,0.2,1.01,0.0731,0.31,0.406,0.0322,1.83e-05,1.97e-05,0.968,0.0712
      Dalton.in.Furness,-726.1,0.3,0.02,4,73.6,257,0.455,28.3,0.203,0.989,0.0386,0.421,0.818,0.0387,2.23e-05,2.36e-05,0.961,0.0779
      Halesworth,-318.6,0.51,0.02,4,49.6,210,0.754,33.1,0.381,0.948,0.00912,0.547,0.641,0.0526,1.99e-05,2.82e-05,0.947,0.0748
      Hastings,-1583.7,0.21,0.02,4,56.3,74.1,0.695,34.2,0.299,1,0.186,0.329,0.396,0.0233,5.61e-06,3.4e-06,0.977,0.0955
      Hull,-2729.4,0.39,0.02,4,42.1,73.9,0.582,38.9,0.221,0.968,0.142,0.275,0.256,0.0371,1.2e-05,1.13e-05,0.963,0.0636
      Leeds,-2918.6,0.23,0.02,4,40.7,35.1,0.666,47.8,0.267,1,1.25,0.592,0.167,0.0262,6.04e-05,3e-05,0.974,0.0778
      Lees,-548.1,1.1,0.02,4,45.6,244,0.612,29.7,0.153,0.968,0.0311,0.648,0.681,0.0477,2.66e-05,2.08e-05,0.952,0.0802
      Liverpool,-3403.1,0.34,0.02,4,49.4,39.3,0.494,48.1,0.305,0.978,0.263,0.191,0.136,0.0286,0.000184,0.00124,0.97,0.0533
      London,-3804.9,0.16,0.02,4,28.9,30.4,0.488,56.8,0.554,0.976,2.9,0.557,0.116,0.0297,5.17e-05,5.14e-05,0.97,0.0878
      Manchester,-3250.9,0.66,0.02,4,34.4,56.8,0.55,32.9,0.29,0.965,0.59,0.362,0.161,0.0489,2.41e-05,3.38e-05,0.951,0.0551
      Mold,-296.5,0.25,0.02,4,67.4,301,0.131,21.4,0.271,1.04,0.0145,0.436,2.87,0.064,2.61e-05,2.27e-05,0.936,0.0544
      Northwich,-1195.1,2.25,0.02,4,45.6,147,0.795,30.1,0.423,0.948,0.0602,0.236,0.402,0.0213,1.32e-05,1.58e-05,0.979,0.0857
      Nottingham,-2703.5,0.53,0.02,4,70.2,115,0.609,22.6,0.157,0.982,0.17,0.34,0.258,0.05,1.36e-05,1.41e-05,0.95,0.038
      Oswestry,-696.1,0.49,0.02,4,37.3,168,0.631,52.9,0.339,1.04,0.0298,0.263,0.476,0.0218,1.56e-05,1.61e-05,0.978,0.0699
      Sheffield,-2810.7,0.21,0.02,4,54.3,62.2,0.649,33.1,0.313,1.02,0.853,0.225,0.175,0.0291,6.04e-05,8.86e-05,0.971,0.0428",
      show_col_types = FALSE
    ) -> mles
  } else {
    demog = alternate_data[["demog"]]
    measles = alternate_data[["measles"]]
    mles = alternate_data[["mles"]]
  }

  ## ----prep-data-------------------------------------------------
  towns = mles[["town"]]
  # Obs list
  dat_list = vector("list", length(towns))
  # Population list
  demog2_list = vector("list", length(towns))
  for(i in seq_along(towns)){
    measles %>%
      mutate(year=as.integer(format(date,"%Y"))) %>%
      filter(town==towns[[i]] & year >= 1950 & year < 1964) %>%
      mutate(
        time=(julian(date, origin = as.Date("1950-01-01")))/365.25 + 1950
      ) %>%
      filter(time > 1950 & time < 1964) %>%
      select(time,cases) -> dat_list[[i]]

    if(is.null(sim_model) == FALSE){
      dat_list[[i]][["cases"]] = as.numeric(obs(sim_model[[i]]))
    }

    demog %>%
      filter(town==towns[[i]]) %>%
      select(-town) -> demog2_list[[i]]
  }
  ## ----prep-covariates-------------------------------------------------
  covar_list = vector("list", length(towns))
  for(i in seq_along(towns)){
    if(AK_interp){
      demog2_list[[i]] %>%
        summarize(
          time=seq(from=min(year),to=max(year),by=1/12),
          pop=predict(smooth.spline(x=year,y=pop),x=time)$y,
          birthrate=predict(smooth.spline(x=year+0.5,y=births),x=time-4)$y
        ) -> covar_list[[i]]
    } else {
      demog2_list[[i]] %>%
        summarize(
          time=seq(from=min(year),to=max(year),by=1/12),
          pop = approx(x = year, y = pop, xout = time)$y,
          birthrate = approx(x = year, y = births, xout = time - 4)$y
        ) -> covar_list[[i]]
    }
    covar_list[[i]] %>%
      mutate(
        pop_1950 = .[.[["time"]] == 1950, "pop"]
      )-> covar_list[[i]]
  }
  for(i in seq_along(towns)){
    covar_list[[i]] = covar_list[[i]] %>%
      mutate(
        std_log_pop_1950 = (log(pop_1950) - mean(sapply(seq_along(towns),
          function(x) log(covar_list[[x]][["pop_1950"]][[1]]))))/
          sd(sapply(seq_along(towns), function(x)
            log(covar_list[[x]][["pop_1950"]][[1]])))
      )
  }

  ## ----pomp-construction-----------------------------------------------
  pomp_list = vector("list", length(towns))
  for(i in seq_along(towns)){
    time = covar_list[[i]][[1]]
    dat_list[[i]] %>%
      pomp::pomp(
        t0 = with(dat_list[[i]], 2*time[1] - time[2]),
        time = "time",
        rprocess = euler(rproc, delta.t = 1/365.25),
        rinit = rinit,
        dmeasure = dmeas,
        rmeasure = rmeas,
        covar = covariate_table(covar_list[[i]], times="time"),
        accumvars = c("C","W"),
        partrans = pt,
        statenames = c("S","E","I","R","C","W"),
        paramnames = paramnames
      ) -> pomp_list[[i]]
  }
  names(pomp_list) = towns

  ## ----panelPomp-construction-----------------------------------------------
  if(is.null(extra_param_start)){
    shared_str_full = shared_str
    shared = colMeans(as.matrix(mles[shared_str]))
  } else {
    shared_str_full = c(shared_str, extra_param_start[["param"]])
    shared = c(colMeans(as.matrix(mles[shared_str])),
               extra_param_start[["value"]])
  }
  specific_str = paramnames[!(paramnames %in% shared_str_full)]
  names(shared) = shared_str_full
  specific = t(as.matrix(mles[mles[["town"]] %in% towns,specific_str]))
  rownames(specific) = specific_str
  colnames(specific) = names(pomp_list)
  panelPomp::panelPomp(
    pomp_list,
    shared = shared,
    specific = specific
  )
}
