test_that("function runs without error", {
  bounds_tbl = tibble::tribble(
    ~param,       ~lower,        ~upper,   ~shared,
    "R0",             10,            60,     FALSE,
    "rho",           0.1,           0.9,     FALSE,
    "sigmaSE",      0.04,           0.1,     FALSE,
    "amplitude",     0.1,           0.6,     FALSE,
    "S_0",          0.01,          0.07,     FALSE,
    "E_0",      0.000004,        0.0001,     FALSE,
    "I_0",      0.000003,         0.001,     FALSE,
    "R_0",           0.9,          0.99,     FALSE,
    "sigma",          25,           100,     FALSE,
    "iota",        0.004,             3,     FALSE,
    "psi",          0.05,             3,     FALSE,
    "alpha",       0.935,          1.05,     FALSE,
    "cohort",        0.1,           0.7,     FALSE,
    "gamma",          25,           320,     FALSE,
    "mu",           0.02,          0.02,      TRUE
  )
  initial_pparams_list = sample_initial_pparams_ul(
    sh_ul = dplyr::filter(bounds_tbl, shared == TRUE),
    sp_ul = dplyr::filter(bounds_tbl, shared == FALSE),
    units = names(AK_mod),
    n_draws = 3
  )
  mass_sample_pparams(
    AK_mod,
    pparams_list = initial_pparams_list,
    Np = 4,
    Nt = 4,
    rank_scheme = "total_ll",
    top_n = 2,
    ncores = 1
  )
  expect_true(TRUE)
})
