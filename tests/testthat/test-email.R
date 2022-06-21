test_that("Single row is parsed nicely for printing", {
  daily <- extract_new_changes(readRDS(test_path("testdata", "test_request.rds")),
                                       readRDS(test_path("testdata", "original.rds")))
  expect_equal(nchar(parse_row(daily[1,])), 195)
})

test_that("Printing works for individual types of changes", {
  daily <- extract_new_changes(readRDS(test_path("testdata", "test_request.rds")),
                               readRDS(test_path("testdata", "original.rds")))
  expect_equal(nchar(print_for_type(daily, 1)), 0)
  expect_equal(nchar(print_for_type(daily)), 217)
})


