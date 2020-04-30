context("get_doaj")

test_that("write output when mining API", {
  df <- tibble::tibble(issn=c("2071-1050","1798-6540","0187-358X","1807-8893","1874-4753"),
                       eissn=rep(NA,5))
  expect_output(get_doaj(df))
})

test_that("DOAJ returns the right ISSNs",{
  issns <- c("2071-1050","1798-6540","0187-358X","1807-8893","1874-4753")
  df <- tibble::tibble(issn=issns,
                       eissn=rep(NA,5))
  doajdf <- get_doaj(df)
  sum(issns %in% doajdf$issn)
  expect(sum(issns %in% doajdf$issn) >= length(issns),
         failure_message = "test ISSNs were missing from DOAJ mining")
})
