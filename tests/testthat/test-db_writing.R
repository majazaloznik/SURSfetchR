dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("mock tests for insert table structures", {
  out <- insert_new_table_structures("1817902S", con, full, schema = "test_platform")
    expect_equal(length(out), 9)
  out <- insert_data_points("1700104S", con, schema = "test_platform")
  expect_true(grepl("0 new rows.*", out))
  })
  dbDisconnect(con)
})

dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "platform",
                        host = "localhost",
                        port = 5433,
                        user = "postgres",
                        password = Sys.getenv("PG_local_15_PG_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("mock tests for insert new dimension levels", {
    out <- add_new_dimension_levels_full("0427602S", 30, con, schema = "test_platform")
    expect_equal(dim(out), c(0,1))
  })
  dbDisconnect(con)
})
