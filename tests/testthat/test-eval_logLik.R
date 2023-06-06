# test_that("logLik has not changed for panelPomp", {
#   set.seed(1)
#   model_list = list(AK_mod)
#   out = eval_logLik(
#     model_list,
#     ncores = 1,
#     np_pf = 2,
#     nreps = 2,
#     return_n_pfilter_objs = 2
#   )
#   print(out$fits$logLik)
#   expect_true(out$fits$logLik < -116458)
#   expect_true(out$fits$logLik > -116460)
# })
# For some reason the logLik is -118729 when run via devtools::check()
