test_that("generalFit.R runs without issues for panelPomp model", {
  # Need to get temporary directory for saving pomp::bake output
  temp_dir = paste0(tempdir(),"/")
  # Change where the output of pomp::bake is saved to
  original_pomp_dir_option = getOption("pomp_archive_dir")
  options(pomp_archive_dir = temp_dir)
  # Need to pass out_dir
  Sys.setenv(out_dir = temp_dir)
  # Correct working directory for devtools::test()
  wd = getwd() |>
    gsub("/tests/testthat$", "", x = _)
  # Correct working directory for devtools::check()
  if(grepl(".Rcheck$", wd)){
    file_loc = paste0(wd, "/measlespkg/misc/generalFit.R")
  } else {
    file_loc = paste0(wd, "/inst/misc/generalFit.R")
  }
  # Actually run the code of interest!
  source(file_loc)
  # Delete saved files from temporary directory.
  # temp_dir needs to be repeated twice due to the contrived manner in which
  # this test must be carried out.
  unlink(paste0(temp_dir,gsub("^/","",temp_dir),"fit_results_out.rds"))
  unlink(paste0(temp_dir,gsub("^/","",temp_dir),"best_eval.rds"))
  # Restore pomp_archive_dir option and system environment
  options(pomp_archive_dir = original_pomp_dir_option)
  Sys.unsetenv("out_dir")
  # Test passes if generalFit.R runs without issue
  expect_true(TRUE)
})
