test_that("output structure is correct", {
  set.seed(1)
  shared_bounds = tibble::tribble(
    ~param,      ~lower,     ~upper,
    "shared1",        0,          1,
    "shared2",        1,          2
  )
  specific_bounds = tibble::tribble(
    ~param,       ~lower,        ~upper,
    "specific1",      10,            20,
    "specific2",      20,            30
  )
  units = c("unit1", "unit2", "unit3")
  out1 = sample_initial_pparams_ul(
    shared_box_specs = shared_bounds,
    specific_box_specs = specific_bounds,
    units = units,
    n_draws = 3
  )
  expect_equal(length(out1), 3)
  expect_equal(names(out1[[1]]), c("shared", "specific"))
  expect_equal(names(out1[[1]]$shared), c("shared1", "shared2"))
  expect_equal(colnames(out1[[1]]$specific), c("unit1", "unit2", "unit3"))
  expect_equal(rownames(out1[[1]]$specific), c("specific1", "specific2"))
  expect_true(inherits(out1[[1]]$shared, "numeric"))
  expect_true(inherits(out1[[1]]$specific, "data.frame"))
})

test_that("output is correct when bounds are equal", {
  set.seed(1)
  shared_bounds = tibble::tribble(
    ~param,      ~lower,     ~upper,
    "shared1",        1,          1,
    "shared2",        2,          2
  )
  specific_bounds = tibble::tribble(
    ~param,       ~lower,        ~upper,
    "specific1",      10,            10,
    "specific2",      20,            20
  )
  units = c("unit1", "unit2", "unit3")
  out1 = sample_initial_pparams_ul(
    shared_box_specs = shared_bounds,
    specific_box_specs = specific_bounds,
    units = units,
    n_draws = 3
  )
  expected1 = list(
    shared = c(shared1 = 1, shared2 = 2),
    specific = data.frame(
      unit1 = c(10, 20),
      unit2 = c(10, 20),
      unit3 = c(10, 20)
    ) |>
      `rownames<-`(c("specific1", "specific2"))
  )
  expect_equal(out1[[1]], expected1)
  expect_equal(out1[[2]], expected1)
  expect_equal(out1[[3]], expected1)
})
