test_that("make_measlesPomp works for model_mechanics_001", {
  make_measlesPomp(
    model_mechanics_001(),
    choose_units(twentycities, c("London", "Mold"))
  )
  expect_true(TRUE)
})

test_that("make_measlesPomp works for model_mechanics_002", {
  make_measlesPomp(
    model_mechanics_002(),
    choose_units(twentycities, c("London", "Mold"))
  )
  expect_true(TRUE)
})

test_that("make_measlesPomp works for model_mechanics_003", {
  make_measlesPomp(
    model_mechanics_003(),
    choose_units(twentycities, c("London", "Mold"))
  )
  expect_true(TRUE)
})

test_that("make_measlesPomp works for model_mechanics_004", {
  make_measlesPomp(
    model_mechanics_004(),
    choose_units(twentycities, c("London", "Mold"))
  )
  expect_true(TRUE)
})

test_that("make_measlesPomp works for model_mechanics_005", {
  make_measlesPomp(
    model_mechanics_005(),
    choose_units(twentycities, c("London", "Mold"))
  )
  expect_true(TRUE)
})

test_that("make_measlesPomp works for model_mechanics_006", {
  make_measlesPomp(
    model_mechanics_006(),
    choose_units(twentycities, c("London", "Mold"))
  )
  expect_true(TRUE)
})

test_that("make_measlesPomp works for model_mechanics_007", {
  make_measlesPomp(
    model_mechanics_007(U = 2),
    choose_units(twentycities, c("London", "Mold"))
  )
  expect_true(TRUE)
})
