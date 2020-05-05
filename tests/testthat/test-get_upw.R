context("get_upw")

test_that("unpaywall mining works",{
  dois <-  c("10.1080/00141844.2017.1362452",
              "10.7554/elife.33864",
              "10.5553/tvp/138820662018021001001",
              "10.5038/1911-9933.12.2.1527",
              "10.15185/izawol.447")
  df <- tibble::tibble(doi=dois)
  expect_output({
    upwdf <- get_upw(df, email = "b.m.i.vreede@uu.nl")})
  expect_true("data.frame" %in% class(upwdf))
  expect_true("oa_color" %in% names(upwdf))
})
