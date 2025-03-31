test_that("expanding level codes", {
  codes <- list(A = c(1, 2))
  labels <- list(A =c( "lejbl", "bla"))
  df <- data.frame(A = "bla", value = 23, MESEC = "2022M02")
  out7 <- recode_labels(1, codes, labels, df)
  expect_equal(out7$A, 2)
})

dittodb::with_mock_db({
  con <- make_test_connection()
  test_that("mock tests for add_meritve_level_units", {
    out1 <- expand_to_level_codes(14, con, schema = "test_platform")
    expect_true(nrow(out1) == 16)
    expect_true(ncol(out1) == 2)
    expect_true(all(is.na(out1$unit_id)))
    out2 <- expand_to_series_titles(14, con, schema = "test_platform")
    expect_true(nrow(out2) == 16)
    expect_true(ncol(out2) == 1)
    out3 <- SURSfetchR:::expand_to_level_codes(2, con, schema = "test_platform") %>%
      dplyr::mutate(unit_id = NA)
    x <- UMARaccessR::sql_get_levels_from_dimension_id(6, con, "test_platform")
    units <- SURSfetchR:::get_unit_levels_from_meritve(x, con, "test_platform")
    out4 <- SURSfetchR:::add_meritve_level_units(out3, 2, units)
    expect_true(nrow(out4) == 168)
    expect_true(ncol(out4) == 5)

  })

  test_that("mock tests for add_valuenotes_level_units", {
    out5 <- SURSfetchR:::expand_to_level_codes(15, con, schema = "test_platform") %>%
      dplyr::mutate(unit_id = NA)
    units <- SURSfetchR:::get_valuenotes_from_px("1700104S", con, "test_platform")
    out6 <- SURSfetchR:::add_valuenotes_level_units(out5, 2, units)
    expect_true(is.data.frame(out6))
  })
})



