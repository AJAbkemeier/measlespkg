test_that("function returns correct output", {
  actual = panelPomp::pparams(AK_mod) |> pparams_to_coef()
  expected = panelPomp::coef(AK_mod)
  expect_equal(
    actual,
    expected[names(actual)]
  )
})
