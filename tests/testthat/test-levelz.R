test_that("Metadata is properly parsed from GET", {
  expect_error(get_surs_metadata("222"))
  mtdt_tbl <- get_surs_metadata("2221702S")
  checkmate::expect_tibble(mtdt_tbl)
  checkmate::expect_list(mtdt_tbl$levels)
  expect_equal(mtdt_tbl, get_surs_metadata("2221702S.px"))
  expect_equal(mtdt_tbl, get_surs_metadata("2221702S.PX"))
})


#
# test_that("Metadata is properly parsed from GET", {
#   expect_error(fill_listcolumn_w_mtdt("222"))
#   matrixez_w_mtdt <- fill_listcolumn_w_mtdt("2221702S")
#   checkmate::expect_tibble(matrixez_w_mtdt)
#
#
#
# })
