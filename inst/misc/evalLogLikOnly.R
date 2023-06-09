Sys.time()
devtools::session_info()
## ----load-packages-----------------------------------------------
# library(measlespkg)
# invisible(sapply(list.files(path = "./R/functions2", pattern = "*.R"),
#                  function(x) source(paste0("./R/functions2/", x))))

## ----setup-----------------------------------------------
ncores = as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE", unset = NA))
if(is.na(ncores)) ncores = parallel::detectCores()/2
print(ncores)

RUN_LEVEL = 2
NP_PF      = switch(RUN_LEVEL, 50, 10000,  10000)
NREPS_EVAL = switch(RUN_LEVEL,  4,     1, ncores)
# new; fit_results
MODEL_OBJ_FROM = "new"
# new; fit_results; ELL
PARAMS_FROM = "fit_results"
LOAD_FIT_FROM = "output2/mod_01/group_04/search_01/round_01/fit_results_out.rds"
LOAD_PARAMS_FROM =
  "output2/mod_01/group_04/search_01/round_01/fit_results_out.rds"
WRITE_ELL_DIR = "output2/mod_01/group_04/search_01/eval_01/"
WRITE_FILE = "logLik_eval.rds"
MAIN_SEED = 7594832
N_PFILTER_OBJS = 1

set.seed(MAIN_SEED)
if(!dir.exists(WRITE_ELL_DIR)) dir.create(WRITE_ELL_DIR)

# Get model object
if(MODEL_OBJ_FROM == "new"){
  model_obj = AK_model()
} else if(MODEL_OBJ_FROM == "fit_results"){
  fitr = readRDS(LOAD_FIT_FROM)
  model_obj = fitr[[1]][[1]]
}

# Get parameters
if(PARAMS_FROM == "new"){
  pomp::coef(model_obj) = pomp::coef(AK_model())
} else if(PARAMS_FROM == "fit_results"){
  fitr = readRDS(LOAD_PARAMS_FROM)
  fits = fitr[[2]][[1]]$fits
  best_fit = unlist(fits[order(fits$logLik),][1, -(1:2)])
  pomp::coef(model_obj) = best_fit
} else if(PARAMS_FROM == "ELL"){
  fitr = readRDS(LOAD_PARAMS_FROM)
  fits = fitr$fits
  best_fit = unlist(fits[order(fits$logLik),][1, -(1:2)])
  pomp::coef(model_obj) = best_fit
}

## ----pfilter-----------------------------------------------
ELL_out = pomp::bake(file = paste0(WRITE_ELL_DIR, WRITE_FILE), {
  eval_logLik(
    model_obj_list = list(model_obj),
    ncores = NREPS_EVAL,
    np_pf = NP_PF,
    nreps = NREPS_EVAL,
    seed = NULL,
    divisor = NULL,
    return_n_pfilter_objs = N_PFILTER_OBJS
  )
})
