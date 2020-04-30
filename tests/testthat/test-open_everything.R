context("open_everything")

test_that("we can open an example file", {
  library(readxl)
  allfilesource <- system.file("extdata", "config_pub_files.xlsx", package = "OAmonitor")
  allfiles <- readxl::read_excel(allfilesource)
  open_everything(allfiles)
})
