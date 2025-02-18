test_that("mock tests for get_meritve_no", {
  dittodb::with_mock_db({
    con <- make_test_connection()
    out <- SURSfetchR:::get_meritve_no(20, con, schema = "test_platform")
    expect_true(out == 2)
  })
})

test_that("mock tests for get_unit_levels_from_meritve", {
  dittodb::with_mock_db({
    con <- make_test_connection()
    level_text_from_meritve <- UMARaccessR::sql_get_levels_from_dimension_id(63, con, schema = "test_platform")
    out <- SURSfetchR:::get_unit_levels_from_meritve(level_text_from_meritve, con, schema = "test_platform")
    expect_true(all(colnames(out) %in% c("tab_dim_id", "level_value", "tmp", "unit", "unit_id")))
  })
})


test_that("mock tests for get_interval_id", {
  out <- get_interval_id("ČETRTLETJE")
  expect_true(length(out) == 1)
  expect_true(out == "Q")
  out <- get_interval_id("MESEC")
  expect_true(length(out) == 1)
  expect_true(out == "M")
  out <- get_interval_id("LETO")
  expect_true(length(out) == 1)
  expect_true(out == "A")
})


