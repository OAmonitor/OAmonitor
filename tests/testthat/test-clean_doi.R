context("clean_doi")

test_that("able to clean DOIs", {
  testdois <- c("https://doi.org/10.1080/00141844.2017.1362452",
                "10.7554/eLife.33864",
                "10.5553/TVP/138820662018021001001",
                "10.5038/1911-9933.12.2.1527",
                "10.15185/izawol.447, 10.15185/izawol.447")
  cleandois <-  c("10.1080/00141844.2017.1362452",
                  "10.7554/elife.33864",
                  "10.5553/tvp/138820662018021001001",
                  "10.5038/1911-9933.12.2.1527",
                  "10.15185/izawol.447")
  expect_equal(clean_doi(testdois),cleandois)
})

test_that("able to detect DOI",{
  expect_true(is.na(clean_doi("5038/1911-9933.12.2.1527")))
})
