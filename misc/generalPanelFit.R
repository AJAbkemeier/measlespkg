Sys.time()
######### load-packages ###############################
# Resolve function name conflict
# select <- dplyr::select
######## Source functions ############################
# invisible(sapply(list.files(path = "./R/functions", pattern = "*.R"),
#                  function(x) source(paste0("./R/functions/", x))))

######## Get arguments from command line #############
cmd_args_list = get_cmd_args()
print(get_cmd_args())
## ############# OPTIONS #############################
# Set number of cores
ncores = as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE", unset = NA))
if(is.na(ncores)) ncores = detectCores()/2
print(ncores)

# Set fitting and filter parameters
RUN_LEVEL = 1
NP_MIF       = switch(RUN_LEVEL, 10, 5000)
NMIF         = switch(RUN_LEVEL, 4,  100)
NREPS_MIF    = switch(RUN_LEVEL, 4,  ncores)
NP_EVAL      = switch(RUN_LEVEL, 10, 10000)
NREPS_EVAL   = switch(RUN_LEVEL, 3,  ncores)

# TOP_N_FITS selects top fits from likelihood evalation file specified in
# PREVIOUS_FIT_PATH. TOP_N_FITS must divide NREPS_MIF.
TOP_N_FITS   = switch(RUN_LEVEL, 2,  12)
BLOCK_MIF2 = TRUE
AK_INTERP = TRUE
# SIM_MODEL specifies whether simulated data from a given model should be used.
SIM_MODEL = NULL
COOLING_FRAC = 0.5
# EVAL_POINTS sets points to evaluate EVAL_PARAM at when performing profile
# search. Set both equal to NULL to do regular search.
EVAL_POINTS = NULL
EVAL_PARAM = NULL
### Seeds ###
# Set seeds other than MAIN_SEED equal to NULL in order to base RNG entirely on
# MAIN_SEED.
MAIN_SEED = 169566665
SIM_MODEL_SEED = NULL
INITIAL_PARAMS_SEED = NULL
MIF2_START_SEED = NULL
MIF2_BAKE_SEED = NULL
PFILTER_EVAL_SEED = NULL
BEST_EVAL_SEED = NULL
PLOT_SIMS_SEED = NULL

(array_job_id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA)))

# Add to MAIN_SEED if running array job
if(!is.na(array_job_id)){
  MAIN_SEED = MAIN_SEED + array_job_id
  print(MAIN_SEED)
}
# Set PREVIOUS_FIT_PATH to NULL to choose starting parameters from a box
# instead of from a previous fit. Setting this equal to a path will nullify
# the portion of code which chooses starting parameters from a box.
PREVIOUS_FIT_PATH = NULL

# Use RS0 to set random walk standard deviations for parameters.
# NOTE: current mif2_alt function might not be able to use different sd for
# different ivp.
DEFAULT_SD = 0.02
IVP_DEFAULT_SD = DEFAULT_SD*3
RS0 = c(
  S_0 = IVP_DEFAULT_SD,
  E_0 = IVP_DEFAULT_SD,
  I_0 = IVP_DEFAULT_SD,
  R_0 = IVP_DEFAULT_SD,
  R0 = DEFAULT_SD,
  sigmaSE = DEFAULT_SD,
  amplitude = DEFAULT_SD,
  rho = DEFAULT_SD,
  gamma = DEFAULT_SD,
  psi = DEFAULT_SD,
  iota = DEFAULT_SD,
  sigma = DEFAULT_SD,
  cohort = DEFAULT_SD,
  alpha = 0.002,
  mu = 0
)
if(!is.null(EVAL_PARAM))
  RS0[[EVAL_PARAM]] = 0

# Specify names of output files
MIF2_FILE = "mif2_out.rds"
LL_FILE = "EL_out.rda"

### Diagnostic parameters ###
# For plots and final loglik calc, use combination of parameters which yields
# best sum of unit loglik?
USE_BEST_COMBO = TRUE
PLOT_TRACES = TRUE
PLOT_SIMS = FALSE
################## SETUP ###########################################
set.seed(MAIN_SEED)
# Create directory for output if it does not exist
write_path = switch(
  RUN_LEVEL,
  "./output/DUMMY/",
  cmd_args_list$path_arg
)
dir.create(write_path)
write_mif2_to = paste0(write_path, MIF2_FILE)
write_LL_to = paste0(write_path, LL_FILE)

# Use observations from simulation?
if(!is.null(SIM_MODEL)){
  sim = simulate(
    SIM_MODEL,
    seed = SIM_MODEL_SEED
  )
  sim_obs_list = lapply(seq_along(sim), function(x){
    pomp::obs(sim[[x]]) |>
      as.numeric()
  })
} else {
  sim_obs_list = NULL
}

################## Construct panelPomp object ##########################
measles_ppomp_mod = make_measlesPomp(
  twentycities,
  starting_pparams = AK_pparams,
  model_mechanics_001(),
  AK_interp = AK_INTERP
)

if(!is.null(EVAL_POINTS)){
  coef_names = paste0(EVAL_PARAM, "[", UNITS, "]")
  coef(measles_ppomp_mod)[coef_names] = EVAL_POINTS[[array_job_id]]
}
########################################################################

# Specify box to sample initial shared parameters from
shared_box_specs = tribble(
  ~param, ~center, ~radius,
  #gamma1",    -0.63695, 0.05,
  #"gamma0",    4.61215,    0.5,
  "mu",        0.02,        0
)

# Specify box to sample initial specific parameters from
specific_radii = tribble(
  ~param, ~radius,
  "R0",        6,
  "rho",       0.05,
  "sigmaSE",   6,
  "amplitude", 0.08,
  "S_0",       0.01,
  "E_0",       0.0001,
  "I_0",       0.001,
  "R_0",       0.01,
  "sigma",     10,
  "iota",      0.5,
  "psi",       0.4,
  "alpha",     0.02,
  "cohort",    0.1,
  "gamma",     30
)
if(!is.null(EVAL_PARAM))
  specific_radii[specific_radii[["param"]] == EVAL_PARAM, "radius"] = 0

specific_bounds = tribble(
  ~param,       ~lower,        ~upper,
  "R0",             10,            60,
  "rho",           0.1,           0.9,
  "sigmaSE",      0.04,           0.1,
  "amplitude",     0.1,           0.6,
  "S_0",          0.01,          0.07,
  "E_0",      0.000004,        0.0001,
  "I_0",      0.000003,         0.001,
  "R_0",           0.9,          0.99,
  "sigma",          25,           100,
  "iota",        0.004,             3,
  "psi",          0.05,             3,
  "alpha",       0.935,          1.05,
  "cohort",        0.1,           0.7,
  "gamma",          25,           320
)
if(!is.null(EVAL_PARAM)){
  eval_param_rows = specific_radii[["param"]] == EVAL_PARAM
  specific_bounds[eval_param_rows, "lower"] = EVAL_POINTS[[array_job_id]]
  specific_bounds[eval_param_rows, "upper"] = EVAL_POINTS[[array_job_id]]
}

if(USE_SPECIFIC_BOUNDS){
  centers = (specific_bounds$lower + specific_bounds$upper)/2
  names(centers) = specific_bounds$param
  specific_centers = construct_specific_pparams(
    pparams_df = pparams(measles_ppomp_mod)[[2]],
    coef_choices = centers
  )
  specific_radii = specific_bounds %>%
    transmute(param = param, radius = (upper-lower)/2)
} else {
  specific_centers = pparams(measles_ppomp_mod)[[2]]
}

specific_box_specs = construct_specific_box_specs(
  specific_pparams = specific_centers,
  radii_tbl = specific_radii
)

# Sample initial parameters and place into lists
param_guess_list = construct_param_guess_list(
  shared_box_specs = shared_box_specs,
  specific_box_specs = specific_box_specs,
  nseq = NREPS_MIF,
  seed = INITIAL_PARAMS_SEED,
  pos_params = c("R0", "mu", "sigmaSE", "psi", "iota", "sigma", "alpha"),
  unit_interval_params = c("S_0", "E_0", "I_0", "R_0", "cohort", "rho",
                           "amplitude")
)


# Get starting parameters from previous fit
if(!is.null(PREVIOUS_FIT_PATH)){
  load(PREVIOUS_FIT_PATH)
  if(length(measles_ppomp_mod) == 1){
    grabbed_params = grab_top_fits(
      EL_out,
      top_n = TOP_N_FITS
    )$fits %>%
      select(!logLik & !se)
  } else {
    grabbed_params = combine_top_fits(
      EL_out,
      top_n = TOP_N_FITS
    )$fits %>%
      select(!logLik & !se)
  }
  top_params = slice(rep(1:nrow(grabbed_params), each = NREPS_MIF/TOP_N_FITS))
  pparam_list = lapply(1:nrow(top_params), function(x){
    coef_to_pparams(top_params[x,])
  })
}

##################### mif2 ##################################
registerDoParallel(cores = ncores)
registerDoRNG(MIF2_START_SEED)
tic()
mif2_alt_list = mif2_alt(
  measles_ppomp_mod = measles_ppomp_mod,
  param_guess_list = param_guess_list,
  rs0 = RS0,
  run_level = RUN_LEVEL,
  nmif = NMIF,
  n_alt = N_ALT,
  nreps_mif2 = NREPS_MIF,
  np_mif2 = NP_MIF,
  cooling_frac = COOLING_FRAC,
  block = BLOCK_MIF2,
  shared_first = SHARED_FIRST,
  seed = MIF2_BAKE_SEED,
  write_mif2_to = write_mif2_to
)
mif2_out = mif2_alt_list[[length(mif2_alt_list)]]
toc()

## ----pfilter-----------------------------------------------
# Note that King log likelihood is −40345.7
# He10 log likelihood from paper SI is -40348.1
EL_out = eval_logLik(
  model_obj_list = mif2_out,
  ncores = ncores,
  np_pf = NP_EVAL,
  nreps = NREPS_EVAL,
  seed = PFILTER_EVAL_SEED,
  divisor = 8164
)
save(EL_out, file = write_LL_to)

# Evaluate at parameters of best ULL combination
if(USE_BEST_COMBO){
  top_params = combine_best_ull(pf_logLik_frame, pf_unitlogLik_frame,
                                pf_unitSE_frame)[-(1:2)]
  eval_model = measles_ppomp_mod
  coef(eval_model) = top_params
  EL_out_best = eval_logLik(
    model_obj_list = list(eval_model),
    ncores = ncores,
    write_to = paste0(write_path, "best_eval.rda"),
    np_pf = NP_EVAL,
    nreps = ncores*8,
    seed = BEST_EVAL_SEED,
    divisor = 8164
  )
  save(EL_out_best, file = paste0(write_path, "pfilter_obj_best_eval.rda"))
}


################ diagnostics ###############################################
if(USE_BEST_COMBO == FALSE){
  top_params = grab_top_params(EL_out)$fits %>%
    dplyr::select(-logLik, -se)
    unlist()
}

if(PLOT_TRACES){
  plot_traces(
    mif2_alt_list,
    plot_shared = "loglik",
    plot_specific = ".ALL",
    print_plots = FALSE,
    save_dir = paste0(write_path, "trace_plots/")
  )
}

if(PLOT_SIMS){
  coef(measles_ppomp_mod) = top_params
  He10_model = measles_ppomp_mod
  set.seed(PLOT_SIMS_SEED)
  sim_plots(
    true_model = He10_model,
    sim_model = measles_ppomp_mod,
    print_plots = FALSE,
    save_dir = paste0(write_path, "sim_plots/")
  )
}


