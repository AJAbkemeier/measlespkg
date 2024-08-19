test_that("parameters are correct", {
  expect_equal(AK_mod@shared, AK_pparams$shared)
  expect_equal(AK_mod@specific, AK_pparams$specific)
})

test_that("data frames are correct", {
  answer = readRDS(test_path("fixtures", "AK_dfs.rds"))
  AK_dfs = lapply(AK_mod@unit_objects, as.data.frame)
  expect_equal(AK_dfs, answer)
})

