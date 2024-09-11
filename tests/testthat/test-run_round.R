test_that("function runs", {
  run_round(
    AK_mod,
    initial_pparams_list = list(AK_pparams),
    np_fitr = 2,
    np_eval = 2,
    ncores = 1,
    cooling_frac = 0.5,
    N_fitr = 2,
    nreps_eval = 2,
    rw_sd_obj = rw_sd(mu = 0, sigma = 0.02, gamma = 0.02, rho = 0.02, R0 = 0.02,
                      amplitude = 0.02, alpha = 0.02, iota = 0.02, cohort = 0.02,
                      psi = 0.02, S_0 = 0.02, E_0 = 0.02, I_0 = 0.02, R_0 = 0.02,
                      sigma = 0.02)
  )
  expect_true(TRUE)
})
