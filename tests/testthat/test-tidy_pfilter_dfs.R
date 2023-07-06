test_that("function produces correct output for panelPomp data", {
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
  out1 = tidy_pfilter_dfs(ell)
  expected1 = data.frame(
    rep = rep(1:3, each = 3),
    total_ll = rep(1:3, each = 3) + 0.1,
    total_se = rep(1:3, each = 3) + 0.2,
    unit = rep(paste0("unit", 1:3)),
    ull = rep(1:3, each = 3) + rep(1:3)/10,
    se = rep(1:3, each = 3) + rep(1:3)/10,
    shared = rep(1:3, each = 3) + 0.3,
    specific = rep(1:3, each = 3) + rep(4:6)/10
  )
  expect_equal(out1, expected1)
})

test_that("function produces correct output for spatPomp data", {
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
  out1 = tidy_pfilter_dfs(ell)
  expected1 = data.frame(
    rep = rep(1:3, each = 3),
    total_ll = rep(1:3, each = 3) + 0.1,
    total_se = rep(1:3, each = 3) + 0.2,
    unit = rep(paste0("unit", 1:3)),
    ull = rep(1:3, each = 3) + rep(1:3)/10,
    se = rep(1:3, each = 3) + rep(1:3)/10,
    shared = rep(1:3, each = 3) + 0.3,
    specific = rep(1:3, each = 3) + rep(4:6)/10
  )
  expect_equal(out1, expected1)
})

