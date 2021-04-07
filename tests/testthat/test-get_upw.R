context("get_upw")

test_that("unpaywall mining works",{
  dois <-  c("10.1080/00141844.2017.1362452",
              "10.7554/elife.33864",
              "10.5553/tvp/138820662018021001001",
              "10.5038/1911-9933.12.2.1527",
              "10.15185/izawol.447",
             "10.1007/ 78-3-319-27857-5_z", #invalid DOI
             "10.14273/unisa-1437" #DataCite DOI
             )
  df <- tibble::tibble(doi=dois)
  expect_output({
    upwdf <- get_upw(df, email = "b.m.r.kramer@uu.nl")})
  # the result is a data frame
  expect_true("data.frame" %in% class(upwdf))
  # the data frame contains the column oa_color
  expect_true("oa_color" %in% names(upwdf))
  # the data frame is as long as all DOIs - but this is not true if invalid DOIs are present
  #expect_true(nrow(upwdf) == length(dois))
})
