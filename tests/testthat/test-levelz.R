test_that("Metadata is properly parsed from GET", {
  expect_error(get_surs_metadata("222"))
  mtdt_tbl <- get_surs_metadata("2221702S")
  checkmate::expect_tibble(mtdt_tbl)
  checkmate::expect_list(mtdt_tbl$levels)
  expect_equal(mtdt_tbl, get_surs_metadata("2221702S.px"))
  expect_equal(mtdt_tbl, get_surs_metadata("2221702S.PX"))
})



test_that("Metadata tibbles are written into a listcolumn", {
  expect_error(fill_listcolumn_w_mtdt("222"))
  api_list <- pxweb::pxweb_get("https://pxweb.stat.si/SiStatData/api/v1/sl/Data")
  df <- as.data.frame(api_list)[1:5,]
  matrixez_w_mtdt <- fill_listcolumn_w_mtdt(df)
  checkmate::expect_tibble(matrixez_w_mtdt$levelz[[1]])
  expect_equal(ncol(df) + 1, ncol(matrixez_w_mtdt))
  x <- data.frame(id = "0156104S")
  fill_listcolumn_w_mtdt(x) -> xx

})
