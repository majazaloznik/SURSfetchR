test_that("Metadata is properly parsed", {
  expect_error(get_px_metadata("222"))
  mtdt_tbl <- get_px_metadata("2221702S")
  checkmate::expect_data_frame(mtdt_tbl)
  checkmate::expect_list(mtdt_tbl$notes)
  expect_equal(mtdt_tbl, get_px_metadata("2221702S.px"))
  expect_equal(mtdt_tbl, get_px_metadata("2221702S.PX"))
  checkmate::expect_posixct(mtdt_tbl$created)
})
