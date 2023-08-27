test_that("function works with gamma", {
  starting_specific =
    AK_pparams$specific[rownames(AK_pparams$specific) != "gamma",] |>
    rbind(AK_pparams$specific[rep("gamma", 20),]) |>
    rbind(30)
  rownames(starting_specific)[14:33] = paste0("gamma",1:20)
  rownames(starting_specific)[[34]] = "w"
  starting_pparams = AK_pparams
  starting_pparams$specific = starting_specific
  starting_pparams$shared = c(mu = 0.02, gamma_sh = 30)
  model = make_measlesPomp(
    data = twentycities,
    starting_pparams = starting_pparams,
    model = model_mechanics_001w("gamma", 20)
  )
  expect_true(
    all(model_mechanics_001w("gamma", 20)$paramnames %in%
                c(names(model@shared), rownames(model@specific)))
  )
})

test_that("function works with amplitude", {
  starting_specific =
    AK_pparams$specific[rownames(AK_pparams$specific) != "amplitude",] |>
    rbind(AK_pparams$specific[rep("amplitude", 20),]) |>
    rbind(30)
  rownames(starting_specific)[14:33] = paste0("amplitude",1:20)
  rownames(starting_specific)[[34]] = "w"
  starting_pparams = AK_pparams
  starting_pparams$specific = starting_specific
  starting_pparams$shared = c(mu = 0.02, amplitude_sh = 30)
  model = make_measlesPomp(
    data = twentycities,
    starting_pparams = starting_pparams,
    model = model_mechanics_001w("amplitude", 20)
  )
  expect_true(
    all(model_mechanics_001w("amplitude", 20)$paramnames %in%
          c(names(model@shared), rownames(model@specific)))
  )
})

