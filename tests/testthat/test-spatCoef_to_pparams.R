test_that("function produces expected output", {
  spatCoef = c(
    sapply(1:20, function(x) 1 + 0.01*x),
    sapply(1:20, function(x) 2 + 0.01*x)
  )
  names(spatCoef) = c(
    sapply(1:20, function(x) paste0("shared",x)),
    sapply(1:20, function(x) paste0("specific",x))
  )
  units = sapply(1:20, function(x) paste0("unit",x))
  out1 = spatCoef_to_pparams(spatCoef, units = units)
  expected1 = list(
    shared = numeric(0),
    specific = data.frame(
      shared = spatCoef[1:20],
      specific = spatCoef[21:40],
      row.names = units
    ) |>
      t() |>
      as.data.frame()
  )
  expect_equal(out1, expected1)
})
