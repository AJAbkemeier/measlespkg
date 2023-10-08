test_that("function runs without error for panelPomp", {
  suppressWarnings(arma_benchmark2(xpomp = AK_mod))
  expect_true(TRUE)
})
