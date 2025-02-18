

test_that("mock tests table prep without db access", {
  dittodb::with_mock_db({
    con <- make_test_connection()
    x <- prepare_vintage_table("1700104S", con, schema = "test_platform")
    expect_true(all(dim(x) == c(12,2)))
    x <- prepare_data_table("1700104S", con, schema = "test_platform")
    expect_equal(dim(x), c(1404 ,4))
  })
})
