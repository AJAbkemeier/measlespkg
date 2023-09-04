test_that("special parameter selection works", {
  U = 20
  rw_sd_vec = c(
    sigma = 0.01,
    w = 0.02,
    mu_sh = 0.03,
    I_0 = 0.04,
    sapply(paste0("mu",1:U), function(x) 0.05)
  )
  out = make_rw_sd(rw_sd_vec, weighted_param = "mu")
  for(i in 1:U){
    expect_equal(out[[i]]@call$sigma, 0.01)
    expect_equal(out[[i]]@call$w, 0.02)
    expect_equal(out[[i]]@call$mu_sh, 0.03)
    expect_true(inherits(out[[i]]@call$I_0, "expression"))
    expect_equal(out[[i]]@call[[paste0("mu",i)]], 0.05)
    for(j in setdiff(1:U, i)){
      expect_equal(out[[i]]@call[[paste0("mu",j)]], 0)
    }
  }
})

test_that("special parameter selection works in weird order", {
  U = 20
  rw_sd_vec = c(
    sigma = 0.01,
    w = 0.02,
    mu_sh = 0.03,
    I_0 = 0.04,
    sapply(paste0("mu",c(1:5, 11:15, 6:10, 16:20)), function(x) 0.05)
  )
  out = make_rw_sd(rw_sd_vec, weighted_param = "mu")
  for(i in 1:U){
    expect_equal(out[[i]]@call$sigma, 0.01)
    expect_equal(out[[i]]@call$w, 0.02)
    expect_equal(out[[i]]@call$mu_sh, 0.03)
    expect_true(inherits(out[[i]]@call$I_0, "expression"))
    expect_equal(out[[i]]@call[[paste0("mu",i)]], 0.05)
    for(j in setdiff(1:U, i)){
      expect_equal(out[[i]]@call[[paste0("mu",j)]], 0)
    }
  }
})
