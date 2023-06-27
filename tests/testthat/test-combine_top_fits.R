test_that("function works for panelPomp output", {
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
      unit1 = c(3.1, 2.1, 1.1),
      unit2 = c(1.2, 3.2, 2.2),
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
  expect_warning(combine_top_fits(ell, top_n = 1, is_spat = FALSE))

  ell$fits$shared = 1

  out1 = combine_top_fits(ell, top_n = 1, is_spat = FALSE)
  out2 = combine_top_fits(ell, top_n = 2, is_spat = FALSE)
  out3 = combine_top_fits(ell, top_n = 3, is_spat = FALSE)

  ### Smallest n
  expect_equal(
    out1$fits$logLik,
    sum(ell$ull[1,1], ell$ull[2,2], ell$ull[3,3])
  )
  expect_equal(
    out1$fits$se,
    sqrt(sum(ell$se[1,1]^2, ell$se[2,2]^2, ell$se[3,3]^2))
  )
  expect_equal(
    out1$fits[3:6],
    dplyr::bind_cols(
      ell$fits[1,3], ell$fits[1,4], ell$fits[2,5], ell$fits[3,6]
    )
  )
  expect_equal(
    out1$ull,
    dplyr::bind_cols(ell$ull[1,1], ell$ull[2,2], ell$ull[3,3])
  )
  expect_equal(
    out1$se,
    dplyr::bind_cols(ell$se[1,1], ell$se[2,2], ell$se[3,3])
  )
  expect_equal(
    out1$cll,
    list(
      unit1 = ell$cll$unit1[1,, drop = FALSE],
      unit2 = ell$cll$unit2[2,, drop = FALSE],
      unit3 = ell$cll$unit3[3,, drop = FALSE]
    )
  )
  expect_equal(
    out1$cll_se,
    list(
      unit1 = ell$cll_se$unit1[1,, drop = FALSE],
      unit2 = ell$cll_se$unit2[2,, drop = FALSE],
      unit3 = ell$cll_se$unit3[3,, drop = FALSE]
    )
  )
  expect_equal(out1$np_pf, ell$np_pf)
  expect_equal(out1$nreps, ell$nreps)

  ### Largest n
  expect_equal(
    out3$fits$logLik,
    c(
      sum(ell$ull[1,1], ell$ull[2,2], ell$ull[3,3]),
      sum(ell$ull[2,1], ell$ull[3,2], ell$ull[2,3]),
      sum(ell$ull[3,1], ell$ull[1,2], ell$ull[1,3])
    )
  )
  expect_equal(
    out3$fits$se,
    c(
      sum(ell$se[1,1]^2, ell$se[2,2]^2, ell$se[3,3]^2),
      sum(ell$se[2,1]^2, ell$se[3,2]^2, ell$se[2,3]^2),
      sum(ell$se[3,1]^2, ell$se[1,2]^2, ell$se[1,3]^2)
    ) |> sqrt()
  )
  expect_equal(
    out3$fits[3:6],
    dplyr::bind_rows(
      dplyr::bind_cols(
        ell$fits[1,3], ell$fits[1,4], ell$fits[2,5], ell$fits[3,6]
      ),
      dplyr::bind_cols(
        ell$fits[1,3], ell$fits[2,4], ell$fits[3,5], ell$fits[2,6]
      ),
      dplyr::bind_cols(
        ell$fits[1,3], ell$fits[3,4], ell$fits[1,5], ell$fits[1,6]
      ),
    )
  )
  expect_equal(
    out3$ull,
    dplyr::bind_rows(
      dplyr::bind_cols(
        ell$ull[1,1], ell$ull[2,2], ell$ull[3,3]
      ),
      dplyr::bind_cols(
        ell$ull[2,1], ell$ull[3,2], ell$ull[2,3]
      ),
      dplyr::bind_cols(
        ell$ull[3,1], ell$ull[1,2], ell$ull[1,3]
      ),
    )
  )
  expect_equal(
    out3$se,
    dplyr::bind_rows(
      dplyr::bind_cols(
        ell$se[1,1], ell$se[2,2], ell$se[3,3]
      ),
      dplyr::bind_cols(
        ell$se[2,1], ell$se[3,2], ell$se[2,3]
      ),
      dplyr::bind_cols(
        ell$se[3,1], ell$se[1,2], ell$se[1,3]
      ),
    )
  )
  expect_equal(
    out3$cll,
    list(
      unit1 = ell$cll$unit1[1:3,, drop = FALSE],
      unit2 = ell$cll$unit2[c(2,3,1),, drop = FALSE],
      unit3 = ell$cll$unit3[3:1,, drop = FALSE]
    )
  )
  expect_equal(
    out3$cll_se,
    list(
      unit1 = ell$cll_se$unit1[1:3,, drop = FALSE],
      unit2 = ell$cll_se$unit2[c(2,3,1),, drop = FALSE],
      unit3 = ell$cll_se$unit3[3:1,, drop = FALSE]
    )
  )
})


test_that("function works for spatPomp output", {
  ell = EL_list(
    fits = data.frame(
      logLik = c(1.1, 2.1, 3.1),
      se = c(1.2, 2.2, 3.2),
      shared1 = c(1.3, 2.3, 3.3),
      shared2 = c(1.3, 2.3, 3.3),
      shared3 = c(1.3, 2.3, 3.3),
      specific1 = c(1.4, 2.4, 3.4),
      specific2 = c(1.5, 2.5, 3.5),
      specific3 = c(1.6, 2.6, 3.6),
      check.names = FALSE
    ),
    ull = data.frame(
      unit1 = c(3.1, 2.1, 1.1),
      unit2 = c(1.2, 3.2, 2.2),
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

  out1 = combine_top_fits(ell, top_n = 1, is_spat = TRUE)
  out2 = combine_top_fits(ell, top_n = 2, is_spat = TRUE)
  out3 = combine_top_fits(ell, top_n = 3, is_spat = TRUE)

  ### Smallest n
  expect_equal(
    out1$fits$logLik,
    sum(ell$ull[1,1], ell$ull[2,2], ell$ull[3,3])
  )
  expect_equal(
    out1$fits$se,
    sqrt(sum(ell$se[1,1]^2, ell$se[2,2]^2, ell$se[3,3]^2))
  )
  expect_equal(
    out1$fits[3:8],
    dplyr::bind_cols(
      ell$fits[1,3], ell$fits[2,4], ell$fits[3,5],
      ell$fits[1,6], ell$fits[2,7], ell$fits[3,8],
    )
  )
  expect_equal(
    out1$ull,
    dplyr::bind_cols(ell$ull[1,1], ell$ull[2,2], ell$ull[3,3])
  )
  expect_equal(
    out1$se,
    dplyr::bind_cols(ell$se[1,1], ell$se[2,2], ell$se[3,3])
  )
  expect_equal(
    out1$cll,
    list(
      unit1 = ell$cll$unit1[1,, drop = FALSE],
      unit2 = ell$cll$unit2[2,, drop = FALSE],
      unit3 = ell$cll$unit3[3,, drop = FALSE]
    )
  )
  expect_equal(
    out1$cll_se,
    list(
      unit1 = ell$cll_se$unit1[1,, drop = FALSE],
      unit2 = ell$cll_se$unit2[2,, drop = FALSE],
      unit3 = ell$cll_se$unit3[3,, drop = FALSE]
    )
  )
  expect_equal(out1$np_pf, ell$np_pf)
  expect_equal(out1$nreps, ell$nreps)

  ### Largest n
  expect_equal(
    out3$fits$logLik,
    c(
      sum(ell$ull[1,1], ell$ull[2,2], ell$ull[3,3]),
      sum(ell$ull[2,1], ell$ull[3,2], ell$ull[2,3]),
      sum(ell$ull[3,1], ell$ull[1,2], ell$ull[1,3])
    )
  )
  expect_equal(
    out3$fits$se,
    c(
      sum(ell$se[1,1]^2, ell$se[2,2]^2, ell$se[3,3]^2),
      sum(ell$se[2,1]^2, ell$se[3,2]^2, ell$se[2,3]^2),
      sum(ell$se[3,1]^2, ell$se[1,2]^2, ell$se[1,3]^2)
    ) |> sqrt()
  )
  expect_equal(
    out3$fits[3:8],
    dplyr::bind_rows(
      dplyr::bind_cols(
        ell$fits[1,3], ell$fits[2,4], ell$fits[3,5],
        ell$fits[1,6], ell$fits[2,7], ell$fits[3,8]
      ),
      dplyr::bind_cols(
        ell$fits[2,3], ell$fits[3,4], ell$fits[2,5],
        ell$fits[2,6], ell$fits[3,7], ell$fits[2,8]
      ),
      dplyr::bind_cols(
        ell$fits[3,3], ell$fits[1,4], ell$fits[1,5],
        ell$fits[3,6], ell$fits[1,7], ell$fits[1,8]
      ),
    )
  )
  expect_equal(
    out3$ull,
    dplyr::bind_rows(
      dplyr::bind_cols(
        ell$ull[1,1], ell$ull[2,2], ell$ull[3,3]
      ),
      dplyr::bind_cols(
        ell$ull[2,1], ell$ull[3,2], ell$ull[2,3]
      ),
      dplyr::bind_cols(
        ell$ull[3,1], ell$ull[1,2], ell$ull[1,3]
      ),
    )
  )
  expect_equal(
    out3$se,
    dplyr::bind_rows(
      dplyr::bind_cols(
        ell$se[1,1], ell$se[2,2], ell$se[3,3]
      ),
      dplyr::bind_cols(
        ell$se[2,1], ell$se[3,2], ell$se[2,3]
      ),
      dplyr::bind_cols(
        ell$se[3,1], ell$se[1,2], ell$se[1,3]
      ),
    )
  )
  expect_equal(
    out3$cll,
    list(
      unit1 = ell$cll$unit1[1:3,, drop = FALSE],
      unit2 = ell$cll$unit2[c(2,3,1),, drop = FALSE],
      unit3 = ell$cll$unit3[3:1,, drop = FALSE]
    )
  )
  expect_equal(
    out3$cll_se,
    list(
      unit1 = ell$cll_se$unit1[1:3,, drop = FALSE],
      unit2 = ell$cll_se$unit2[c(2,3,1),, drop = FALSE],
      unit3 = ell$cll_se$unit3[3:1,, drop = FALSE]
    )
  )
})
