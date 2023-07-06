test_that("function produces expected output", {
  pp_coef = c(
    sapply(1:5, function(x) 1 + 0.01*x),
    sapply(1:10, function(x) 2 + 0.01*x),
    sapply(1:10, function(x) 3 + 0.01*x)
  )
  names(pp_coef) = c(
    sapply(1:5, function(x) paste0("shared",x)),
    sapply(1:10, function(x) paste0("specific1[unit",x,"]")),
    sapply(1:10, function(x) paste0("specific2[unit",x,"]"))
  )
  units = sapply(1:10, function(x) paste0("unit",x))
  out1 = coef_to_pparams(pp_coef)
  expected1 = list(
    shared = pp_coef[1:5],
    specific = data.frame(
      specific1 = pp_coef[1:10 + 5],
      specific2 = pp_coef[1:10 + 5 + 10],
      row.names = units
    ) |>
      t() |>
      as.data.frame()
  )
  expect_equal(out1, expected1)
})
