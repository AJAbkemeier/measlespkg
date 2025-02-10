test_that("function works", {
  suppressWarnings(ar1NB_benchmark(AK_mod))
  suppressWarnings(ar1NB_benchmark(obs2(AK_mod)))
  expect_true(TRUE)
})
