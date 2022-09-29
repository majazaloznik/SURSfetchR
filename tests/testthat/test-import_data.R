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
    x <- get_last_publication_date(1, con)
    expect_true(inherits(x, "Date"))
    expect_true(x == "2022-02-02")
  })
})




