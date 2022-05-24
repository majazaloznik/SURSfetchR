

test_that("html parsing is returning a relevant table", {
  df <- parse_surs_updates()
  expect_equal(ncol(df), 2)
  expect_lte(nrow(df), 3709)
  expect_true(lubridate::is.Date(df$date_updated))
  df <- subset_parsed_df(df, "2022-05-23")
  expect_equal(nrow(df), 16)
})

test_that("we're getting the correct data from the OPSI API", {
  df <- surs_opsi_api(date = "2022-05-23")
  expect_equal(nrow(df), 16)
  expect_equal(ncol(df), 3)
  df <- surs_opsi_api(date = "2022-05-23", table = "New")
  expect_equal(df, NA)
})

