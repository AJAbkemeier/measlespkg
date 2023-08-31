test_that("function works with gamma", {
  mm = model_mechanics_001w("gamma", 20)
  model = make_measlesPomp(
    data = twentycities,
    starting_pparams = NULL,
    model = mm
  )
  expect_true(
    all(mm$paramnames %in% c(names(model@shared), rownames(model@specific)))
  )
})

test_that("function works with amplitude", {
  mm = model_mechanics_001w("amplitude", 20)
  model = make_measlesPomp(
    data = twentycities,
    starting_pparams = NULL,
    model = mm
  )
  expect_true(
    all(mm$paramnames %in% c(names(model@shared), rownames(model@specific)))
  )
})

