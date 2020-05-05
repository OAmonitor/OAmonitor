context("open_everything")

test_that("can open example data correctly", {
  allfilesource <- system.file("extdata", "config_pub_files.xlsx", package = "OAmonitor")
  datafiles <- system.file("extdata", "", package = "OAmonitor")
  df <- open_everything(file = allfilesource, dir = datafiles)
  expect_true("tbl" %in% class(df))
  expect_true(nrow(df) == 1136)
  expect_true(ncol(df) == 17)
})
