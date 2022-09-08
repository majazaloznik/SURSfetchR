dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbSendQuery(con, "set search_path to test_platform")

  test_that("mock tests work", {
    expect_true(length(get_table_id("0300230S", con)) == 1)
    expect_true(length(get_table_id("x", con)) == 0)
    expect_true(length(get_unit_id("%", con)) == 1)
    expect_true(get_unit_id("%", con) == 2)
  })
  dbDisconnect(con)
})
