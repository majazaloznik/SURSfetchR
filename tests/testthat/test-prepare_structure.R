
full <- readRDS(test_path("testdata", "full_h.rds"))

test_that("mock tests table prep without db access", {
  x <- prepare_table_table("1700104S")
  expect_true(all(dim(x) == c(1,6)))
  x <- prepare_category_table("1700104S", full)
  expect_true(all(dim(x) == c(6,3)))
  x <- prepare_category_relationship_table("1700104S", full)
  expect_true(all(dim(x) == c(6,3)))
  x <- prepare_unit_table("1700104S")
  expect_true(all(dim(x) == c(2,1)))
  expect_true(length(unique(x[,1])) == 2)
})


test_that("mock tests for table prep with db access", {
  dittodb::with_mock_db({
    con <- make_test_connection()
    x <- prepare_category_table_table("1700104S", full, con, schema = "test_platform")
    expect_true(all(dim(x) == c(1,3)))
    expect_true(x$category_id == 321)
    x <- prepare_table_dimensions_table("1700104S", con, schema = "test_platform")
    expect_true(all(dim(x) == c(3,3)))
    expect_true(x$is_time[1])
    x <- prepare_dimension_levels_table("1700104S", con, schema = "test_platform")
    expect_true(all(dim(x) == c(8,4)))
    expect_true(max(x$level_value) == 6)
    x <- prepare_series_table("1700104S", con, schema = "test_platform")
    expect_true(ncol(x) == 5)
    expect_true("SURS--1700104S--1--1--Q" %in% x$series_code)
    x <- prepare_series_levels_table("1700104S", con, "test_platform")
    expect_true(all(dim(x) == c(24,3)))
  })
})

