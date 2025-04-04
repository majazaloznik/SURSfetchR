test_that("SURS_import_structure works correctly with mocked dimension_selector", {
  with_mock_db({
      con <- make_test_connection()
      result <- SURS_import_structure("2711808S", con, schema = "test_platform")
    expect_true("dimension_levels" %in% names(result))
    expect_true(is.list(result$dimension_levels))
    expect_true("count" %in% names(result$dimension_levels))
  })
})

test_that("SURS_import_data_points works correctly ", {
  with_mock_db({
    con <- make_test_connection()
    x <- SURS_import_data_points("0714621S", con, schema = "test_platform")
    expect_true("data" %in% names(x))
    expect_true("vintages" %in% names(x))

  })
})
