context("get_doaj")

test_that("write output when mining API", {
  df <- tibble::tibble(issn=c("2071-1050","1798-6540","0187-358X","1807-8893"))
  expect_output(get_doaj(df, source="api"))
})
