library(dittodb)
full <- readRDS(test_path("testdata", "full_h.rds"))

test_that("Metadata is properly parsed", {
  expect_error(get_px_metadata("222"))
  mtdt_tbl <- get_px_metadata("2221702S")
  checkmate::expect_data_frame(mtdt_tbl)
  expect_true(inherits(mtdt_tbl$notes, "json"))
  expect_equal(mtdt_tbl, get_px_metadata("2221702S.px"))
  expect_equal(mtdt_tbl, get_px_metadata("2221702S.PX"))
  checkmate::expect_posixct(mtdt_tbl$updated)
  expect_true(ncol(mtdt_tbl)== 8)
})

test_that("Data is downloaded ok. ", {
  expect_error(get_px_data("222"))
  dt_tbl <- get_px_data("0811602S")
  checkmate::expect_list(dt_tbl)
  expect_true(length(dt_tbl) == 3)
  expect_true(ncol(dt_tbl[[1]]) == 4)
  expect_true(nrow(dt_tbl[[1]]) >= 540)
})

test_that("Metadata is properly parsed from GET", {
  expect_error(get_table_levels_from_px("222"))
  mtdt_tbl <- get_table_levels_from_px("2221702S")
  checkmate::expect_tibble(mtdt_tbl)
  checkmate::expect_list(mtdt_tbl$levels)
  expect_equal(mtdt_tbl, get_table_levels_from_px("2221702S.px"))
  expect_equal(mtdt_tbl, get_table_levels_from_px("2221702S.PX"))
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

test_that("newlines are removed from descriptions", {
  x <- get_px_metadata("2001301S")$name
  expect_true(nchar(x) < 286)

})

test_that("mock tests valuenotes functions", {
  dittodb::with_mock_db({
    con <- make_test_connection()
    x <- get_px_metadata("1700104S")$valuenotes[[1]][1]
    out <- SURSfetchR:::get_valuenotes_unit(x, con, "test_platform")
    expect_equal(out, 3)
    x <- SURSfetchR:::get_single_unit_from_px("0457101S", con, "test_platform")
    expect_true(x == 8)
  })
})



