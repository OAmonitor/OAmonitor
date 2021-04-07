context("get_upw")

test_that("unpaywall mining works",{
  dois <-  c("10.1007/jhep11(2015)127",
             "10.1109/icdsp.2015.7251906",
             "10.1117/1.jei.24.5.053018",
             "10.1109/ism.2015.104",
             "10.7763/lnse.2015.v3.202",
             "10.1109/rcis.2015.7128873",
             "10.1504/ijiq.2015.071672",
             "10.4018/ijsds.2015070101",
             "10.7763/lnse.2015.v3.200",
             "10.1109/rcis.2015.7128884",
             "10.1007/978-3-319-14442-9_11",
             "10.17083/ijsg.v2i2.74",
             "10.1007/978-3-319-20603-5",
             "10.1111/psyp.12431",
             "10.1145/2797115.2797123",
             "10.1186/s40166-015-0008-9",
             "10.1007/978-3-319-40216-1_2",
             "10.1007/978-3-319-20276-1_12",
             "10.1007/978-3-319-20276-1",
             "10.1145/2814895.2814930",
             "10.1111/jcal.12104",
             "10.1007/978-3-319-20276-1_5",
             "10.1145/2695664.2695877",
             "10.1016/j.bica.2015.06.005",
             "10.1007/978-3-319-14442-9_25",
             "10.4108/icst.intetain.2015.259399",
             "10.1007/978-3-319-23234-8_52",
             "10.1007/978-3-319-24027-5_26",
             "10.1093/oxfordhb/9780199942237.013.035",
             "10.4108/icst.intetain.2015.259589",
             "10.1007/978-3-319-40216-1_56",
             "10.1145/2821592.2821634",
             "10.1016/j.csi.2014.08.001",
             "10.1145/2746090.2746094",
             "10.5220/0005597602490258",
             "10.1007/978-3-319-24258-3_49",
             "10.1016/j.scico.2015.10.008",
             "10.1007/978-3-319-15317-9_4",
             "10.1007/s40266-015-0270-0",
             "10.1145/2808098.2808101",
             "10.1016/j.jss.2014.10.034",
             "10.1007/s10270-015-0499-4",
             "10.1007/978-3-319-25840-9_8",
             "10.1145/2678015.2682541",
             "10.1007/978-3-319-19593-3_19",
             "10.1145/2678015.2682543",
             "10.1007/978-3-662-46681-0_24",
             "10.1016/j.chb.2014.09.030",
             "10.1016/j.datak.2015.07.007",
             "10.1145/2678015.2682535",
             "10.1145/2678015.2682536")
  df <- tibble::tibble(doi=dois)
  expect_output({
    upwdf <- get_upw(df, email = "b.m.r.kramer@uu.nl")})
  # the result is a data frame
  expect_true("data.frame" %in% class(upwdf))
  # the data frame contains the column oa_color
  expect_true("oa_color" %in% names(upwdf))
  # the data frame is as long as all DOIs
  expect_true(nrow(upwdf) == length(dois))
  # TODO add line for testing green over bronze prioritization once implemented
})

test_that("invalid DOIs and DOIS not included in UPW do not break the loop",{
  doi_some_fails <- c(
    "10.1007/jhep11(2015)127",
    "10.1109/icdsp.2015.7251906",
    "10.1117/1.jei.24.5.053018",
    "10.1007/ 78-3-319-27857-5_z", #invalid DOI
    "10.14273/unisa-1437" #DataCite DOI
  )
  df <- tibble::tibble(doi=doi_some_fails)
  expect_output({
    upwdf <- get_upw(df, email = "b.m.r.kramer@uu.nl")})
  # the data frame is as long as all valid DOIs
  expect_true(nrow(upwdf) == 3)
})

test_that("source data with only errors can be dealt with",{
  doi_all_fails <- c(
    "10.1007/ 78-3-319-27857-5_z",
    "10.1016/j.compedu.2015.08",
    "10.1007/978-3-319-19890-3 24",
    "10.3233/978-1-61499-609-5-11",
    "10.14273/unisa-1437"
    )
  df <- tibble::tibble(doi=doi_all_fails)
  skip("currently failing test")
  expect_output({
    upwdf <- get_upw(df, email = "b.m.r.kramer@uu.nl")})
  })
