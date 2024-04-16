test_that("function works", {
  data_list = list(
    unit1 = data.frame(
      time = 1:50,
      Y = rnorm(50)
    ),
    unit2 = data.frame(
      time = 1:50,
      Y = rnorm(50)
    )
  )

  tp1 = make_toyPomp(
    model = toy_mechanics_001(),
    data_list = data_list
  )

  sp = list(
    shared = NULL,
    specific = matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  )
  rownames(sp$specific) = c("sigma", "mu")
  colnames(sp$specific) = c("unit1", "unit2")

  tp2 = make_toyPomp(
    model = toy_mechanics_001(),
    data_list = data_list,
    starting_pparams = sp
  )
  expect_true(TRUE)
})


test_that("function works with run_round()",{
  data_list = list(
    unit1 = data.frame(
      time = 1:50,
      Y = rnorm(50)
    ),
    unit2 = data.frame(
      time = 1:50,
      Y = rnorm(50)
    )
  )

  sp1 = list(
    shared = NULL,
    specific = matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  )
  rownames(sp1$specific) = c("sigma", "mu")
  colnames(sp1$specific) = c("unit1", "unit2")

  tp1 = make_toyPomp(
    model = toy_mechanics_001(),
    data_list = data_list
  )

  out = run_round(
    tp1,
    initial_pparams_list = list(sp1, sp1),
    write_results_to = tempfile(),
    ncores = 2,
    np_fitr = 2,
    cooling_frac = 0.5,
    rw_sd_obj = make_rw_sd(c(mu = 0.02, sigma = 0.02)),
    N_fitr = 2,
    np_eval = 2,
    panel_block = TRUE,
    nreps_eval = 2
  )

  sp2 = list(
    shared = 3,
    specific = matrix(c(1, 2), nrow = 1, ncol = 2)
  )
  names(sp2$shared) = "mu"
  rownames(sp2$specific) = c("sigma")
  colnames(sp2$specific) = c("unit1", "unit2")

  tp2 = make_toyPomp(
    model = toy_mechanics_001(shared_params = "mu"),
    data_list = data_list
  )

  out = run_round(
    tp2,
    initial_pparams_list = list(sp2, sp2),
    write_results_to = tempfile(),
    ncores = 2,
    np_fitr = 2,
    cooling_frac = 0.5,
    rw_sd_obj = make_rw_sd(c(mu = 0.02, sigma = 0.02)),
    N_fitr = 2,
    np_eval = 2,
    panel_block = TRUE,
    nreps_eval = 2
  )

  expect_true(TRUE)
})








