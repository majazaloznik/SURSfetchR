full <- readRDS(test_path("testdata", "full_h.rds"))

test_that("Metadata is properly parsed", {
  expect_error(get_px_metadata("222"))
  mtdt_tbl <- get_px_metadata("2221702S")
  checkmate::expect_data_frame(mtdt_tbl)
  expect_true(inherits(mtdt_tbl$notes, "json"))
  expect_equal(mtdt_tbl, get_px_metadata("2221702S.px"))
  expect_equal(mtdt_tbl, get_px_metadata("2221702S.PX"))
  checkmate::expect_posixct(mtdt_tbl$created)
  expect_true(ncol(mtdt_tbl)== 7)
})


test_that("Categories are properly parsed", {
  x <- get_row(12964, full)
  checkmate::expect_data_frame(x)
  expect_true(nrow(x) == 7)
  expect_true(ncol(x) == 4)
  x <- get_row("lkj", full)
  expect_true(nrow(x) == 0)
 expect_error(get_row(123, x))
})
