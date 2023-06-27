test_that("function works", {
  ell = EL_list(
    fits = data.frame(
      logLik = c(1.1, 2.1, 3.1),
      se = c(1.2, 2.2, 3.2),
      shared = c(1.3, 2.3, 3.3),
      `specific[unit1]` = c(1.4, 2.4, 3.4),
      `specific[unit2]` = c(1.5, 2.5, 3.5),
      `specific[unit3]` = c(1.6, 2.6, 3.6),
      check.names = FALSE
    ),
    ull = data.frame(
      unit1 = c(1.1, 2.1, 3.1),
      unit2 = c(1.2, 2.2, 3.2),
      unit3 = c(1.3, 2.3, 3.3)
    ),
    se = data.frame(
      unit1 = c(1.1, 2.1, 3.1),
      unit2 = c(1.2, 2.2, 3.2),
      unit3 = c(1.3, 2.3, 3.3)
    ),
    cll = lapply(1:3, function(x){
      mat = matrix(
        10*x + c(1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3),
        nrow = 3,
        ncol = 3
      )
    }) |> `names<-`(c("unit1", "unit2", "unit3")),
    cll_se = lapply(1:3, function(x){
      matrix(
        10*x + c(1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3),
        nrow = 3,
        ncol = 3
      )
    }) |> `names<-`(c("unit1", "unit2", "unit3")),
    np_pf = 10,
    nreps = 20
  )
  out1 = grab_top_fits(ell, top_n = 1)
  out2 = grab_top_fits(ell, top_n = 2)
  out3 = grab_top_fits(ell, top_n = 3)

  expect_equal(out1$fits, ell$fits[3,])
  expect_equal(out1$ull, ell$ull[3,])
  expect_equal(out1$se, ell$se[3,])
  expect_equal(out1$cll$unit1, ell$cll$unit1[3,])
  expect_equal(out1$cll$unit2, ell$cll$unit2[3,])
  expect_equal(out1$cll$unit3, ell$cll$unit3[3,])
  expect_equal(out1$np_pf, ell$np_pf)
  expect_equal(out1$nreps, ell$nreps)

  expect_equal(out2$fits, ell$fits[3:2,])
  expect_equal(out2$ull, ell$ull[3:2,])
  expect_equal(out2$se, ell$se[3:2,])
  expect_equal(out2$cll$unit1, ell$cll$unit1[3:2,])
  expect_equal(out2$cll$unit2, ell$cll$unit2[3:2,])
  expect_equal(out2$cll$unit3, ell$cll$unit3[3:2,])

  expect_equal(out3$fits, ell$fits[3:1,])
  expect_equal(out3$ull, ell$ull[3:1,])
  expect_equal(out3$se, ell$se[3:1,])
  expect_equal(out3$cll$unit1, ell$cll$unit1[3:1,])
  expect_equal(out3$cll$unit2, ell$cll$unit2[3:1,])
  expect_equal(out3$cll$unit3, ell$cll$unit3[3:1,])

  expect_equal(colnames(out3$fits), colnames(ell$fits))
  expect_equal(colnames(out3$ull), colnames(ell$ull))
  expect_equal(colnames(out3$se), colnames(ell$se))
  expect_equal(names(out3$cll), names(ell$cll))
  expect_equal(names(out3$cll_se), names(ell$cll_se))
})
