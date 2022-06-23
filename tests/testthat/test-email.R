test_that("Single row is parsed nicely for printing", {
  daily <- extract_new_changes(readRDS(test_path("testdata", "test_request.rds")),
                                       readRDS(test_path("testdata", "original.rds")))
  expect_equal(nchar(parse_row(daily[1,])), 211)
})

test_that("Printing works for individual types of changes", {
  daily <- extract_new_changes(readRDS(test_path("testdata", "test_request.rds")),
                               readRDS(test_path("testdata", "original.rds")))
  expect_equal(nchar(print_for_type(daily, 1)), 0)
  expect_equal(nchar(print_for_type(daily)), 254)
})

test_that("Email body is rendered correctly", {
  changes <- extract_new_changes(readRDS(test_path("testdata", "test_request.rds")),
                               readRDS(test_path("testdata", "original.rds")))
  new_df <- readRDS(test_path("testdata", "test_request.rds"))
  today <- extract_todays_changes(new_df, date = "2022-06-23")
  df <- email_surs_changes_body(changes, today)
  expect_equal(nchar(df), 2080)
})

