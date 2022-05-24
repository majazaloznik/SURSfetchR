test_that("html parsing is returning a relevant table", {
  df <- parse_surs_updates()
  expect_equal(ncol(df), 2)
  expect_lte(nrow(df), 3709)
  expect_true(lubridate::is.Date(df$date_updated))
  df <- subset_parsed_df(df, "2022-05-23")
  expect_lte(nrow(df), 16)
})
