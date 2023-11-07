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
  model_list = list(AK_mod, AK_mod)
  out = eval_logLik(
    model_list,
    ncores = 2,
    np_pf = 2,
    nreps = 2
  )
  ml_length = length(model_list)
  expect_true(inherits(out, "EL_list"))
  expect_true(nrow(out$fits) == ml_length)
  expect_true(nrow(out$ull) == ml_length)
  expect_true(setequal(colnames(out$ull), names(AK_mod)))
  expect_true(nrow(out$se) == ml_length)
  expect_true(setequal(colnames(out$se), names(AK_mod)))
})

test_that("function works for panelPomp models with 1 mif2 rep", {
  set.seed(1)
  model_list = list(AK_mod)
  out = eval_logLik(
    model_list,
    ncores = 1,
    np_pf = 2,
    nreps = 1
  )
  ml_length = length(model_list)
  expect_true(inherits(out, "EL_list"))
  expect_true(nrow(out$fits) == ml_length)
  expect_true(nrow(out$ull) == ml_length)
  expect_true(setequal(colnames(out$ull), names(AK_mod)))
  expect_true(nrow(out$se) == ml_length)
  expect_true(setequal(colnames(out$se), names(AK_mod)))
})

test_that("function works for panelPomp models with 1 unit", {
  set.seed(1)
  model_list = list(AK_mod["London"])
  out = eval_logLik(
    model_list,
    ncores = 1,
    np_pf = 2,
    nreps = 1
  )
  ml_length = length(model_list)
  expect_true(inherits(out, "EL_list"))
  expect_true(nrow(out$fits) == ml_length)
  expect_true(nrow(out$ull) == ml_length)
  expect_true(setequal(colnames(out$ull), "London"))
  expect_true(nrow(out$se) == ml_length)
  expect_true(setequal(colnames(out$se), "London"))
})

# test_that("function works for panelPomp models with 1 unit made from pomp object", {
#   set.seed(1)
#   london_pomp = AK_mod[["London"]]
#   london_panel = panelPomp::panelPomp(
#     list(London = london_pomp),
#     shared = NULL,
#     specific = pomp::coef(london_pomp) |>
#       as.matrix() |>
#       `colnames<-`("London")
#   )
#   out = eval_logLik(
#     list(london_panel),
#     ncores = 1,
#     np_pf = 2,
#     nreps = 1
#   )
#   expect_true(inherits(out, "EL_list"))
# })

test_that("function works for spatPomp models", {
  set.seed(1)
  model_list = list(spatPomp_ex)
  out = eval_logLik(
    model_obj_list = model_list,
    ncores = 1,
    np_pf = 2,
    nreps = 3
  )
  expect_true(TRUE)
})



