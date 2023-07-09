test_that("function works for panelPomp", {
  out1 = make_sim_tbl(AK_mod, n_sims = 2)
  out2 = make_sim_tbl(AK_mod, n_sims = 2, true_model = AK_mod)
  expect_true(TRUE)
})

test_that("function works for spatPomp", {
  out1 = make_sim_tbl(spatPomp_ex, n_sims = 2)
  out2 = make_sim_tbl(spatPomp_ex, n_sims = 2, true_model = spatPomp_ex)
  expect_true(TRUE)
})
