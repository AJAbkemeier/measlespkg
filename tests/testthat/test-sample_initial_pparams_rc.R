test_that("function produces correct structure", {
  set.seed(1)
  shared_box_specs = tibble::tribble(
    ~param,    ~center, ~radius,
    "shared1",       1,     0.25,
    "shared2",       2,     0.25
  )
  specific_radii = tibble::tribble(
    ~param,       ~radius,
    "specific1",     0.25,
    "specific2",     0.25
  )
  specific_pparams_df = data.frame(
    unit1 = c(1, 10),
    unit2 = c(2, 20),
    unit3 = c(3, 30)
  ) |>
    `rownames<-`(c("specific1", "specific2"))
  out1 = sample_initial_pparams_rc(
    shared_box_specs = shared_box_specs,
    specific_pparams_df = specific_pparams_df,
    radii_tbl = specific_radii,
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

test_that("function produces correct values when radius is 0", {
  set.seed(1)
  shared_box_specs = tibble::tribble(
    ~param,    ~center, ~radius,
    "shared1",       1,       0,
    "shared2",       2,       0
  )
  specific_radii = tibble::tribble(
    ~param,       ~radius,
    "specific1",        0,
    "specific2",        0
  )
  specific_pparams_df = data.frame(
    unit1 = c(1, 10),
    unit2 = c(2, 20),
    unit3 = c(3, 30)
  ) |>
    `rownames<-`(c("specific1", "specific2"))
  out1 = sample_initial_pparams_rc(
    shared_box_specs = shared_box_specs,
    specific_pparams_df = specific_pparams_df,
    radii_tbl = specific_radii,
    n_draws = 3
  )
  expected1 = list(
    shared = c(shared1 = 1, shared2 = 2),
    specific = specific_pparams_df
  )
  expect_equal(out1[[1]], expected1)
  expect_equal(out1[[2]], expected1)
  expect_equal(out1[[3]], expected1)
})
