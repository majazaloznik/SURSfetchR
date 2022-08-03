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
  checkmate::expect_tibble(matrixez_w_mtdt$levelz[[1]])
  expect_equal(ncol(df) + 5, ncol(matrixez_w_mtdt))
  x <- data.frame(id = "0156104S")
  fill_listcolumn_w_mtdt(x) -> xx
})

test_that("Field and matrix and level hierarch works.", {
  join <- matrix_n_level_hierarchy(mat_h,subset = df)
  expect_true(nrow(join) == nrow(df))

})
