context("open_everything")

test_that("opening example files", {
  library(readxl)
  allfilesource <- system.file("extdata", "config_pub_files.xlsx", package = "OAmonitor")
  datafiles <- system.file("extdata", "", package = "OAmonitor")
  df <- open_everything(file = allfilesource, dir = datafiles)
  expect_true("tbl" %in% class(df))
})
