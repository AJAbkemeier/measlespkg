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

test_that("function produces expected output with He10 parameters", {
  U = 20
  spatCoef = c(
    1 + 0.01*1:U,
    2 + 0.01*1:U,
    3 + 0.01*1:U,
    4 + 0.01*1:U,
    5 + 0.01*1:U,
    6 + 0.01*1:U,
    7 + 0.01*1:U,
    8 + 0.01*1:U,
    9 + 0.01*1:U,
    10 + 0.01*1:U,
    11 + 0.01*1:U,
    12 + 0.01*1:U,
    13 + 0.01*1:U,
    14 + 0.01*1:U,
    15 + 0.01*1:U
  )
  names(spatCoef) = c(
    paste0("g",1:U),
    paste0("R0",1:U),
    paste0("rho",1:U),
    paste0("sigmaSE",1:U),
    paste0("amplitude",1:U),
    paste0("S_0",1:U),
    paste0("E_0",1:U),
    paste0("I_0",1:U),
    paste0("sigma",1:U),
    paste0("iota",1:U),
    paste0("psi",1:U),
    paste0("alpha",1:U),
    paste0("cohort",1:U),
    paste0("gamma",1:U),
    paste0("muD",1:U)
  )
  units = c('Bedwellty', 'Birmingham', 'Bradford', 'Bristol', 'Cardiff',
            'Consett', 'Dalton.in.Furness', 'Halesworth', 'Hastings', 'Hull',
            'Leeds', 'Lees', 'Liverpool', 'London', 'Manchester', 'Mold',
            'Northwich', 'Nottingham', 'Oswestry', 'Sheffield')
  out1 = spatCoef_to_pparams(spatCoef, units = units)
  expected1 = list(
    shared = numeric(0),
    specific = data.frame(
      g = spatCoef[paste0("g",1:U)],
      R0 = spatCoef[paste0("R0",1:U)],
      rho = spatCoef[paste0("rho",1:U)],
      sigmaSE = spatCoef[paste0("sigmaSE",1:U)],
      amplitude = spatCoef[paste0("amplitude",1:U)],
      S_0 = spatCoef[paste0("S_0",1:U)],
      E_0 = spatCoef[paste0("E_0",1:U)],
      I_0 = spatCoef[paste0("I_0",1:U)],
      sigma = spatCoef[paste0("sigma",1:U)],
      iota = spatCoef[paste0("iota",1:U)],
      psi = spatCoef[paste0("psi",1:U)],
      alpha = spatCoef[paste0("alpha",1:U)],
      cohort = spatCoef[paste0("cohort",1:U)],
      gamma = spatCoef[paste0("gamma",1:U)],
      muD = spatCoef[paste0("muD",1:U)],
      row.names = units
    ) |>
      t() |>
      as.data.frame()
  )
  expected1$specific = expected1$specific[sort(rownames(expected1$specific)),]
  expect_equal(out1, expected1)

  # Check if output is correct when the units are not enumerated alphabetically
  set.seed(1)
  units2 = units[sample(1:U, size = U)]
  out2 = spatCoef_to_pparams(spatCoef, units = units2)
  expected2 = expected1
  colnames(expected2$specific) = units2
  expect_equal(out2, expected2)
})
