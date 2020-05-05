context("deduplicate_per_unit")

test_that("deduplication is performed accurately",{
  df <- tibble::tibble(
    doi = c("10.1080/00141844.2017.1362452",
            NA,
            "10.7554/elife.33864",
            "10.5553/tvp/138820662018021001001",
            "10.5553/tvp/138820662018021001001",
            "10.5553/tvp/138820662018021001001",
            NA,
            "10.5038/1911-9933.12.2.1527",
            "10.5038/1911-9933.12.2.1527",
            "10.15185/izawol.447",
            "10.15185/izawol.447",
            "10.15185/izawol.447",
            NA
    ),
    system_id = c("A1","A1","B2","A3","B3","B3","A4","B4","A4","A5","A1","A5","A1"),
    source = c("source1","source1","source2","source1","source2","source2",
               "source1","source2","source1","source1","source2","source1","source2"),
    org_unit = c("A","A","A","B","A","B","B","A","A","B","A","B","A")
  )
  df_dedup <- deduplicate_per_unit(df)
  expect_true("tbl"%in%class(df_dedup))
  expect_true(nrow(df_dedup) == 8)
})
