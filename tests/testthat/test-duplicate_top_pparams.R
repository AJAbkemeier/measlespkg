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
  out1 = duplicate_top_pparams(ell, out_length = 4, top_n = 1, combine = FALSE)
  out3 = duplicate_top_pparams(ell, out_length = 6, top_n = 3, combine = FALSE)

  expect_equal(
    out1,
    lapply(1:4, function(x){
      list(
        shared = ell$fits[3,"shared", drop = TRUE] |> `names<-`("shared"),
        specific =
          ell$fits[3, c("specific[unit1]","specific[unit2]","specific[unit3]")] |>
          as.data.frame() |>
          `colnames<-`(c("unit1", "unit2", "unit3")) |>
          `rownames<-`("specific")
      )
    })
  )
  expect_equal(
    out3,
    lapply(3:1, function(x){
      list(
        shared = ell$fits[x,"shared", drop = TRUE] |> `names<-`("shared"),
        specific =
          ell$fits[x, c("specific[unit1]","specific[unit2]","specific[unit3]")] |>
          as.data.frame() |>
          `colnames<-`(c("unit1", "unit2", "unit3")) |>
          `rownames<-`("specific")
      )
    }) |> rep(2)
  )

  ell$fits$shared = 1

  out1c = duplicate_top_pparams(ell, out_length = 4, top_n = 1, combine = TRUE)
  out3c = duplicate_top_pparams(ell, out_length = 6, top_n = 3, combine = TRUE)

  expect_equal(
    out1c,
    lapply(1:4, function(x){
      list(
        shared = ell$fits[3,"shared", drop = TRUE] |> `names<-`("shared"),
        specific =
          dplyr::bind_cols(
            ell$fits[1, "specific[unit1]"],
            ell$fits[2, "specific[unit2]"],
            ell$fits[3, "specific[unit3]"],
          ) |>
          as.data.frame() |>
          `colnames<-`(c("unit1", "unit2", "unit3")) |>
          `rownames<-`("specific")
      )
    })
  )
  expect_equal(
    out3c,
    lapply(1:3, function(x){
      z = list(c(1,2,3), c(2,3,2), c(3,1,1))
      list(
        shared = ell$fits[3,"shared", drop = TRUE] |> `names<-`("shared"),
        specific =
          dplyr::bind_cols(
            ell$fits[z[[x]][[1]], "specific[unit1]"],
            ell$fits[z[[x]][[2]], "specific[unit2]"],
            ell$fits[z[[x]][[3]], "specific[unit3]"],
          ) |>
          as.data.frame() |>
          `colnames<-`(c("unit1", "unit2", "unit3")) |>
          `rownames<-`("specific")
      )
    }) |> rep(2)
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

  out1 = duplicate_top_pparams(
    ell, out_length = 4, top_n = 1, combine = FALSE,
    units = c("unit1", "unit2", "unit3")
  )
  out3 = duplicate_top_pparams(
    ell, out_length = 6, top_n = 3, combine = FALSE,
    units = c("unit1", "unit2", "unit3")
  )

  expect_equal(
    out1,
    lapply(1:4, function(x){
      specific_df = ell$fits[3, c("specific1", "specific2", "specific3")] |>
        as.data.frame() |>
        `colnames<-`(c("unit1", "unit2", "unit3")) |>
        `rownames<-`("specific")
      shared_df = ell$fits[3, c("shared1", "shared2", "shared3")] |>
        as.data.frame() |>
        `colnames<-`(c("unit1", "unit2", "unit3")) |>
        `rownames<-`("shared")
      list(
        shared = numeric(0),
        specific = dplyr::bind_rows(shared_df, specific_df)
      )
    })
  )
  expect_equal(
    out3,
    lapply(3:1, function(x){
      specific_df = ell$fits[x, c("specific1", "specific2", "specific3")] |>
        as.data.frame() |>
        `colnames<-`(c("unit1", "unit2", "unit3")) |>
        `rownames<-`("specific")
      shared_df = ell$fits[x, c("shared1", "shared2", "shared3")] |>
        as.data.frame() |>
        `colnames<-`(c("unit1", "unit2", "unit3")) |>
        `rownames<-`("shared")
      list(
        shared = numeric(0),
        specific = dplyr::bind_rows(shared_df, specific_df)
      )
    }) |> rep(2)
  )

  out1c = duplicate_top_pparams(
    ell, out_length = 4, top_n = 1, combine = TRUE,
    units = c("unit1", "unit2", "unit3")
  )
  out3c = duplicate_top_pparams(
    ell, out_length = 6, top_n = 3, combine = TRUE,
    units = c("unit1", "unit2", "unit3")
  )

  expect_equal(
    out1c,
    lapply(1:4, function(x){
      z = list(c(1,2,3), c(2,3,2), c(3,1,1))
      specific_df = dplyr::bind_cols(
        ell$fits[z[[1]][[1]], "specific1"],
        ell$fits[z[[1]][[2]], "specific2"],
        ell$fits[z[[1]][[3]], "specific3"],
      ) |>
        as.data.frame() |>
        `colnames<-`(c("unit1", "unit2", "unit3")) |>
        `rownames<-`("specific")
      shared_df = dplyr::bind_cols(
        ell$fits[z[[1]][[1]], "shared1"],
        ell$fits[z[[1]][[2]], "shared2"],
        ell$fits[z[[1]][[3]], "shared3"],
      ) |>
        as.data.frame() |>
        `colnames<-`(c("unit1", "unit2", "unit3")) |>
        `rownames<-`("shared")
      list(
        shared = numeric(0),
        specific = dplyr::bind_rows(shared_df, specific_df)
      )
    })
  )
  expect_equal(
    out3c,
    lapply(1:3, function(x){
      z = list(c(1,2,3), c(2,3,2), c(3,1,1))
      specific_df = dplyr::bind_cols(
        ell$fits[z[[x]][[1]], "specific1"],
        ell$fits[z[[x]][[2]], "specific2"],
        ell$fits[z[[x]][[3]], "specific3"],
      ) |>
        as.data.frame() |>
        `colnames<-`(c("unit1", "unit2", "unit3")) |>
        `rownames<-`("specific")
      shared_df = dplyr::bind_cols(
        ell$fits[z[[x]][[1]], "shared1"],
        ell$fits[z[[x]][[2]], "shared2"],
        ell$fits[z[[x]][[3]], "shared3"],
      ) |>
        as.data.frame() |>
        `colnames<-`(c("unit1", "unit2", "unit3")) |>
        `rownames<-`("shared")
      list(
        shared = numeric(0),
        specific = dplyr::bind_rows(shared_df, specific_df)
      )
    }) |> rep(2)
  )
})


test_that("function doesn't mix parameter between units for spatPomp output with
          many units",
{
  U = 35
  units = paste0("unit",1:U)
  ell = EL_list(
    fits = data.frame(
      logLik = c(1.1, 2.1, 3.1),
      se = c(1.2, 2.2, 3.2),
      check.names = FALSE
    ) |> dplyr::bind_cols(
      lapply(1:U, function(i) c(1, 2, 3)) |>
        `names<-`(paste0("shared",1:U)),
      lapply(1:U, function(i) c(1, 2, 3) + i/100) |>
        `names<-`(paste0("specific",1:U))
    ),
    ull = dplyr::bind_cols(
      lapply(1:U, function(i) c(1, 2, 3) + i/100) |>
        `names<-`(units)
    ),
    se = dplyr::bind_cols(
      lapply(1:U, function(i) c(1, 2, 3) + i/100) |>
        `names<-`(units)
    ),
    cll = lapply(1:U, function(x){
      mat = matrix(
        10*x + c(1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3),
        nrow = 3,
        ncol = 3
      )
    }) |> `names<-`(units),
    cll_se = lapply(1:U, function(x){
      matrix(
        10*x + c(1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3),
        nrow = 3,
        ncol = 3
      )
    }) |> `names<-`(units),
    np_pf = 10,
    nreps = 20
  )

  out1 = duplicate_top_pparams(
    ell, out_length = 3, top_n = 1, combine = FALSE,
    units = units
  )
  out2 = duplicate_top_pparams(
    ell, out_length = 3, top_n = 3, combine = FALSE,
    units = units
  )

  expect_equal(
    out1[[1]]$specific["specific",] |> unlist(),
    (3 + 1:U/100) |> `names<-`(units)
  )
  expect_equal(
    out2[[3]]$specific["specific",] |> unlist(),
    (1 + 1:U/100) |> `names<-`(units)
  )

  out1c = duplicate_top_pparams(
    ell, out_length = 3, top_n = 1, combine = TRUE,
    units = units
  )
  out2c = duplicate_top_pparams(
    ell, out_length = 3, top_n = 3, combine = TRUE,
    units = units
  )

  expect_equal(
    out1c[[1]]$specific["specific",] |> unlist(),
    (3 + 1:U/100) |> `names<-`(units)
  )
  expect_equal(
    out2c[[3]]$specific["specific",] |> unlist(),
    (1 + 1:U/100) |> `names<-`(units)
  )

})

