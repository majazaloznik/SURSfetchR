full <- readRDS(test_path("testdata", "full_h.rds"))
dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("mock tests for insert table structures", {
    expect_warning(
      expect_warning(
        expect_warning(out <- insert_new_table_structures("1817902S", con, full))))
    expect_equal(length(out), 9)
  })
  dbDisconnect(con)
})

