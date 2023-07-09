test_that("function works", {
  spat_out = make_spatMeaslesPomp(
    data = clean_twentycities(),
    model = model_mechanics_007(20, "g")
  )
  expect_true(TRUE)
})
