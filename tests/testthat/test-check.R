

test_that("html parsing is returning a relevant table", {
  df <- parse_surs_updates()
  expect_equal(ncol(df), 2)
  expect_gte(nrow(df), 3709)
  expect_true(lubridate::is.Date(df$date_updated))
})

test_that("subsetting is working ok", {
  df <- subset_parsed_df(parse_surs_updates(), date = "2022-01-04")
  expect_equal(nrow(df), 0)
  df <- parse_surs_updates()
  expect_equal(nrow(df), nrow(subset_parsed_df(df, date = NULL)))
})

test_that("we're getting the correct data from the OPSI API", {
  df <- surs_opsi_api(date = "2022-05-23")
  # expect_equal(nrow(df), 16)
  expect_equal(ncol(df), 3)
  suppressMessages(df <- surs_opsi_api(date = "2022-05-23", table = "New"))
  expect_equal(df, NA)
  df <- surs_opsi_api(date = "2022-06-16", table = "New")
  expect_equal(nrow(df), 1)
})


test_that("API requests are parsed correctly", {
  df <- surs_change_api()
  expect_equal(ncol(df), 7)
  suppressMessages(df <- surs_change_api(body = paste0("{'Date': '", date, "'}")))
  expect_equal(df, NA)
})

test_that("New changes are correctly found", {
  new_df <- readRDS(test_path("testdata", "test_request.rds"))
  old_df <- readRDS(test_path("testdata", "original.rds"))
  df <- extract_new_changes(new_df,
                            old_df)
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 7)
  expect_true(any(grepl("1817607S", df$sporociloSlo, 7)))
})

test_that("New changes are correctly mreged with old", {
  new_df <- readRDS(test_path("testdata", "test_request.rds"))
  old_df <- readRDS(test_path("testdata", "original.rds"))
  df <- extract_new_changes(new_df,
                            old_df)
  df <- update_change_table(old_df, df)
  expect_equal(nrow(df), 13)
  expect_equal(ncol(df), 8)
})

