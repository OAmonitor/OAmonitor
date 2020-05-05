context("get_doaj")

test_that("DOAJ returns the right ISSNs",{
  issns <- c("2071-1050","1798-6540","0187-358X","1807-8893","1874-4753")
  df <- tibble::tibble(issn=issns,
                       eissn=rep(NA,5))
  expect_output({
    doajdf <- get_doaj(df)})
  expect_true(sum(issns %in% doajdf$issn) >= length(issns))
})
