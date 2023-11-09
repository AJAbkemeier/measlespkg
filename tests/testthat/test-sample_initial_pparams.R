test_that("function works correctly", {
  set.seed(1)
  shared_bounds = tibble::tribble(
    ~param,      ~lower,     ~upper,
    "shared1",        0,          1
  )
  specific_bounds = tibble::tribble(
    ~param,       ~lower,        ~upper,
    "specific1",      10,            20
  )
  units = c("unit1", "unit2", "unit3")
  shared_box_specs = tibble::tribble(
    ~param,    ~center, ~radius,
    "shared2",       2,     0.25
  )
  specific_radii = tibble::tribble(
    ~param,       ~radius,
    "specific2",     0.25
  )
  specific_pparams_df = data.frame(
    unit1 = 10,
    unit2 = 20,
    unit3 = 30
  ) |>
    `rownames<-`("specific2")

  # Use both methods
  out1 = sample_initial_pparams(
    sh_ul = shared_bounds,
    sp_ul = specific_bounds,
    units_ul = units,
    sh_rc = shared_box_specs,
    sp_c = specific_pparams_df,
    sp_r = specific_radii,
    n_draws = 4
  )
  expect_true(length(out1) == 4)
  expect_true(setequal(names(out1[[1]]$shared), c("shared1", "shared2")))
  expect_true(
    setequal(rownames(out1[[1]]$specific), c("specific1", "specific2"))
  )
  expect_true(setequal(colnames(out1[[1]]$specific), paste0("unit",1:3)))

  # Runs with ul only
  out2 = sample_initial_pparams(
    sh_ul = shared_bounds,
    sp_ul = specific_bounds,
    units_ul = units,
    n_draws = 4
  )
  # Runs with rc only
  out3 = sample_initial_pparams(
    sh_rc = shared_box_specs,
    sp_c = specific_pparams_df,
    sp_r = specific_radii,
    n_draws = 4
  )
  # Fails with partial ul
  expect_error(
    sample_initial_pparams(
      sh_ul = shared_bounds,
      sp_ul = specific_bounds,
      sh_rc = shared_box_specs,
      sp_c = specific_pparams_df,
      sp_r = specific_radii,
      n_draws = 4
    )
  )
  # Fails with partial rc
  expect_error(
    sample_initial_pparams(
      sh_ul = shared_bounds,
      sp_ul = specific_bounds,
      units_ul = units,
      sp_c = specific_pparams_df,
      sp_r = specific_radii,
      n_draws = 4
    )
  )

  # Fails when parameter shows up in both ul and rc arguments
  shared_bounds = tibble::tribble(
    ~param,      ~lower,     ~upper,
    "shared1",        0,          1,
    "shared2",        0,          1
  )
  specific_bounds = tibble::tribble(
    ~param,       ~lower,        ~upper,
    "specific1",      10,            20,
    "specific2",      10,            20
  )
  expect_error(
    sample_initial_pparams(
      sh_ul = shared_bounds,
      sp_ul = specific_bounds,
      units_ul = units,
      sh_rc = shared_box_specs,
      sp_c = specific_pparams_df,
      sp_r = specific_radii,
      n_draws = 4
    )
  )
})
