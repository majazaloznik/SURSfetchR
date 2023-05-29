dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("mock tests for reading & executing sql files", {
    out <- execute_sql_functions_file(con, file = test_path("testdata", "sql_fun_test.sql"))
    expect_equal(out[[1]], 0)
    out <- DBI::dbGetQuery(con, "SELECT Sum(10, 10)")
    expect_equal(out, data.frame(sum = 20))
    out <- execute_sql_file(con, file = test_path("testdata", "sql_nofun_test.sql"))
    expect_equal(out[[1]], 0)
    out <- sql_function_call(con, "sum", list(10,10))
    expect_equal(out, data.frame(sum = 20))
  })

  dbDisconnect(con)
})


