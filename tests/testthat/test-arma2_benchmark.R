test_that("function runs without error for panelPomp", {
  suppressWarnings(arma2_benchmark(x = AK_mod))
  suppressWarnings(arma2_benchmark(x = obs2(AK_mod)))
  expect_true(TRUE)
})
