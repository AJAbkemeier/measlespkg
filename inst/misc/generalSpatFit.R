Sys.time()
devtools::session_info()
######### load-packages ###############################
devtools::load_all()
#library(measlespkg)
library(foreach)
######## Source functions ############################
# invisible(sapply(list.files(path = "./R/functions", pattern = "*.R"),
#                  function(x) source(paste0("./R/functions/", x))))

######## Get arguments from command line #############
(out_dir = as.character(Sys.getenv("out_dir", unset = NA)))
(array_job_id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA)))
## ############# OPTIONS #############################
# Set number of cores
ncores = as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE", unset = NA))
if(is.na(ncores)) ncores = parallel::detectCores()/4
print(ncores)

# Set fitting and filter parameters
RUN_LEVEL = 1
NP_MIF       = switch(RUN_LEVEL, 4, 5000)
NMIF         = switch(RUN_LEVEL, 4,  100)
NREPS_MIF    = switch(RUN_LEVEL, ncores, ncores)
NP_EVAL      = switch(RUN_LEVEL, 4, 10000)
NREPS_EVAL   = switch(RUN_LEVEL, ncores, ncores)
NREPS_EVAL2  = switch(RUN_LEVEL, ncores, ncores*8)
# TOP_N_FITS selects top fits from likelihood evaluation file specified in
# PREVIOUS_FIT_PATH. TOP_N_FITS must divide NREPS_MIF.
TOP_N_FITS   = switch(RUN_LEVEL, 2,  12)
DATA = twentycities
UNITS = unique(twentycities$measles$unit)
MODEL = model_mechanics_007(length(UNITS), shared_names = "g")
DT = 1/365.25
BLOCK_SIZE = 1
SPAT_REGR = 0.1

INTERP_METHOD = "shifted_splines"
# SIM_MODEL specifies whether simulated data from a given model should be used.
SIM_MODEL = NULL
COOLING_FRAC = 0.5
# EVAL_POINTS sets points to evaluate EVAL_PARAM at when performing profile
# search. Set both equal to NULL to do regular search.
EVAL_POINTS = NULL
EVAL_PARAM = NULL

MAIN_SEED = 169566665
# Add to MAIN_SEED if running array job
if(!is.na(array_job_id)){
  MAIN_SEED = MAIN_SEED + array_job_id
  print(MAIN_SEED)
}

# Set PREVIOUS_FIT_PATH to NULL to choose starting parameters from a box
# instead of from a previous fit. Setting this equal to a path will nullify
# the portion of code which chooses starting parameters from a box.
PREVIOUS_FIT_PATH = NULL
COMBINE_TOP_SPECIFIC = FALSE

# Use INITIAL_RW_SD to set random walk standard deviations for parameters.
DEFAULT_SD = 0.02
IVP_DEFAULT_SD = DEFAULT_SD*12
INITIAL_RW_SD = c(
  S_0 = IVP_DEFAULT_SD,
  E_0 = IVP_DEFAULT_SD,
  I_0 = IVP_DEFAULT_SD,
  R0 = DEFAULT_SD,
  sigmaSE = DEFAULT_SD,
  amplitude = DEFAULT_SD*0.5,
  rho = DEFAULT_SD*0.5,
  gamma = DEFAULT_SD*0.5,
  psi = DEFAULT_SD*0.25,
  iota = DEFAULT_SD,
  sigma = DEFAULT_SD,
  cohort = DEFAULT_SD*0.5,
  alpha = DEFAULT_SD*10^(-2),
  g = DEFAULT_SD*0.5,
  muD = 0
)
if(!is.null(EVAL_PARAM))
  INITIAL_RW_SD[[EVAL_PARAM]] = 0

# Specify names of output files
RESULTS_FILE = "fit_results_out.rds"

# For final loglik calc, use combination of parameters which yields best sum of
# unit loglik?
USE_BEST_COMBO = FALSE

################## SETUP ###########################################
set.seed(MAIN_SEED)
# Create directory for output if it does not exist
write_path = switch(
  RUN_LEVEL,
  "./output2/DUMMY/",
  out_dir
)
if(!dir.exists(write_path)) dir.create(write_path)
write_results_to = paste0(write_path, RESULTS_FILE)

# Use observations from simulation?
if(!is.null(SIM_MODEL)){
  sim = simulate(SIM_MODEL, seed = SIM_MODEL_SEED)
  sim_obs_list = lapply(seq_along(sim), function(x){
    pomp::obs(sim[[x]]) |>
      as.numeric()
  })
} else {
  sim_obs_list = NULL
}

###### Starting parameters #############################

# Specify box to sample initial shared parameters from
shared_box_specs = tibble::tribble(
  ~param, ~center,   ~radius,
  #"muD",        0.02,        0
  # "g"  ,         350,     450
)

# Specify box to sample initial specific parameters from
specific_radii = tibble::tribble(
  ~param, ~radius,
  "R0",        6,
  "rho",       0.05,
  "sigmaSE",   6,
  "amplitude", 0.08,
  "S_0",       0.01,
  "E_0",       0.0001,
  "I_0",       0.001,
  "sigma",     10,
  "iota",      0.5,
  "psi",       0.4,
  "alpha",     0.02,
  "cohort",    0.1,
  "gamma",     30,
)
if(!is.null(EVAL_PARAM))
  specific_radii[specific_radii[["param"]] == EVAL_PARAM, "radius"] = 0

shared_bounds = tibble::tribble(
  ~param,    ~lower,   ~upper,
  # "muD",        0.02,     0.02,
  # "g"  ,         350,     450
)

specific_bounds = tibble::tribble(
  ~param,       ~lower,        ~upper,
  "R0",             10,            60,
  "rho",           0.1,           0.9,
  "sigmaSE",      0.04,           0.1,
  "amplitude",     0.1,           0.6,
  "S_0",          0.01,          0.07,
  "E_0",      0.000004,        0.0001,
  "I_0",      0.000003,         0.001,
  "sigma",          25,           100,
  "iota",        0.004,             3,
  "psi",          0.05,             3,
  "alpha",       0.935,          1.05,
  "cohort",        0.1,           0.7,
  "gamma",          25,           320,
  "muD",          0.02,          0.02,
  "g"  ,           350,           450
)
if(!is.null(EVAL_PARAM)){
  eval_param_rows = specific_bounds[["param"]] == EVAL_PARAM
  specific_bounds[eval_param_rows, "lower"] = EVAL_POINTS[[array_job_id]]
  specific_bounds[eval_param_rows, "upper"] = EVAL_POINTS[[array_job_id]]
}

# Sample initial parameters and place into lists
initial_pparams_list = sample_initial_pparams_ul(
  shared_box_specs = shared_bounds,
  specific_box_specs = specific_bounds,
  units = UNITS,
  n_draws = NREPS_MIF
)

# Get starting parameters from previous fit
if(!is.null(PREVIOUS_FIT_PATH)){
  fit_results_in = readRDS(PREVIOUS_FIT_PATH)
  EL_in = fit_results_in$EL_out[[length(fit_results_in$EL_out)]]
  initial_pparams_list = duplicate_top_pparams(
    EL_in,
    out_length = NREPS_MIF,
    top_n = TOP_N_FITS,
    combine = COMBINE_TOP_SPECIFIC,
    units = UNITS
  )
}

################## Construct spatPomp object ##########################
measlesPomp_mod = make_spatMeaslesPomp(
  data = DATA |> choose_units(UNITS),
  model = MODEL,
  interp_method = INTERP_METHOD,
  dt = DT
)

U = length(UNITS)
expanded_rw_sd = unlist(lapply(names(INITIAL_RW_SD), function(x){
    z = rep(INITIAL_RW_SD[[x]], U)
    names(z) = paste0(x,1:U)
    z
}))

if(!is.null(EVAL_POINTS)){
  coef_names = paste0(EVAL_PARAM, "[", UNITS, "]")
  panelPomp::coef(measlesPomp_mod)[coef_names] = EVAL_POINTS[[array_job_id]]
}
###### MODEL FITTING #####################################
round_out = run_round(
  measlesPomp_mod,
  initial_pparams_list = initial_pparams_list,
  rw_sd_obj = make_rw_sd(expanded_rw_sd),
  cooling_frac = COOLING_FRAC,
  N_fitr = NMIF,
  np_fitr = NP_MIF,
  np_eval = NP_EVAL,
  nreps_eval = NREPS_EVAL,
  ncores = ncores,
  write_results_to = write_results_to,
  print_times = TRUE,
  spat_block_size = BLOCK_SIZE,
  spat_regression = SPAT_REGR,
  spat_sharedParNames = MODEL$shp_names,
  spat_unitParNames = MODEL$spp_names
)

EL_final = round_out$EL_out[[length(round_out$EL_out)]]
print(as.data.frame(dplyr::arrange(EL_final$fits[,1:2], dplyr::desc(logLik))))

# Evaluate at parameters of best ULL combination
if(USE_BEST_COMBO){
  tictoc::tic()
  top_params = combine_top_fits(EL_final)$fits[-(1:2)]
  eval_model = measlesPomp_mod
  panelPomp::coef(eval_model) = top_params
  EL_out_best = pomp::bake(file = paste0(write_path, "best_eval.rds"),
    eval_logLik(
    model_obj_list = list(eval_model),
      ncores = ncores,
      np_pf = NP_EVAL,
      nreps = NREPS_EVAL2,
      seed = NULL,
      divisor = 8164
    )
  )
  tictoc::toc()
  as.data.frame(EL_out_best$fits[,1:2])
}
