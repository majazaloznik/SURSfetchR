test_that("mock tests table prep without db access", {
  dittodb::with_mock_db({
    con <- make_test_connection()
    x <- prepare_vintage_table("2711808S", con, schema = "test_platform")
    expect_true(all(dim(x) == c(84,2)))
    x <- prepare_data_table("1700104S", con, schema = "test_platform")
    expect_equal(dim(x), c(1404 ,4))
    x <- prepare_data_table("H240S", con, "test_platform")
    expect_equal(dim(x), c(20 ,4))
  })
})
