dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("preparing tables", {
    x <- prepare_vintage_table("1700104S", con)
    expect_true(all(dim(x) == c(12,2)))
    x <- prepare_data_table("1700104S", con)
    expect_true(all(dim(x) == c(1296,4)))
  })
})




