matrixez_w_mtdt <- readRDS(test_path("testdata", "matrixez_w_mtdt.rds"))
mat_h <- readRDS(test_path("testdata", "mat_h.rds"))

test_that("Metadata is properly parsed from GET", {
  expect_error(get_table_levels_from_px("222"))
  mtdt_tbl <- get_table_levels_from_px("2221702S")
  checkmate::expect_tibble(mtdt_tbl)
  checkmate::expect_list(mtdt_tbl$levels)
  expect_equal(mtdt_tbl, get_table_levels_from_px("2221702S.px"))
  expect_equal(mtdt_tbl, get_table_levels_from_px("2221702S.PX"))
})


test_that("Metadata tibbles are written into a listcolumn", {
  expect_error(fill_listcolumn_w_levelz("222"))
  checkmate::expect_tibble(matrixez_w_mtdt$levelz[[1]])
  x <- data.frame(id = c("0156104S"), updated = "2022-05-30T10:30:00")
  xx <- fill_listcolumn_w_levelz(x)
  xxx <- pull_levels(xx)
  expect_equal(ncol(xxx), 9)
  expect_true(inherits(xxx$updated_file, "POSIXct"))
})

test_that("Field and matrix and level hierarch works.", {
  join <- matrix_n_level_join(mat_h, matrixez_w_mtdt, archive = TRUE, time = FALSE)
  expect_true(nrow(join) == 5)
  expect_false(any(is.na(join$id)))
  expect_false(any(is.na(join$time)))
  full <- full_hierarchy_unnest(join)
  expect_true(ncol(join) + 3 == ncol(full))
  expect_true(all(full$no_points >= full$no_series))
  full <- full %>%
    dplyr::rowwise() %>%
    dplyr::mutate(level_combos = list(get_level_combos(levelz, dim_names_notime))) %>%
    tidyr::unnest(level_combos)
  expect_true(ncol(full) == 26)
  expect_true(nrow(full) == 3459)
})
