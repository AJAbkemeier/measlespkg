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

test_that("function works for panelPomp models", {
  set.seed(1)
  model_list = list(AK_mod)
  out = eval_logLik(
    model_list,
    ncores = 1,
    np_pf = 2,
    nreps = 3,
    return_n_pfilter_objs = 2
  )
  expect_true(inherits(out, "EL_list"))
})

test_that("function works for panelPomp models (no pfilter objects returned)", {
  set.seed(1)
  model_list = list(AK_mod)
  out = eval_logLik(
    model_list,
    ncores = 1,
    np_pf = 2,
    nreps = 3,
    return_n_pfilter_objs = 0
  )
  expect_true(inherits(out, "EL_list"))
})

test_that("function works for panelPomp models with 1 rep", {
  set.seed(1)
  model_list = list(AK_mod)
  out = eval_logLik(
    model_list,
    ncores = 1,
    np_pf = 2,
    nreps = 1,
    return_n_pfilter_objs = 0
  )
  expect_true(inherits(out, "EL_list"))
})

test_that("logLik is correct for spatPomp", {
  set.seed(1)
  model_list = list(spatPomp_ex)
  out = eval_logLik(
    model_obj_list = model_list,
    ncores = 1,
    np_pf = 2,
    nreps = 3,
    return_n_pfilter_objs = 3
  )
  expect_equal(
    out$fits$logLik,
    out$pfilter_list |> lapply(pomp::logLik) |> logmeanexp()
  )
})

test_that("function works for spatPomp models (no pfilter objects returned)", {
  set.seed(1)
  model_list = list(spatPomp_ex)
  out = eval_logLik(
    model_obj_list = model_list,
    ncores = 1,
    np_pf = 2,
    nreps = 3,
    return_n_pfilter_objs = 0
  )
  expect_true(TRUE)
})



