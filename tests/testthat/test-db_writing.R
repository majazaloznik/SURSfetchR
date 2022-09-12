full <- readRDS(test_path("testdata", "full_h.rds"))

dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbSendQuery(con, "set search_path to test_platform")

  test_that("writing to db tables - mock", {
    x <- write_row_table ("0300230S", con, paste("INSERT INTO \"table\"",
                                                 "(code, name, source_id, url, notes)",
                                                 "VALUES",
                                                 "($1, $2, $3, $4, $5)"), 0)
    expect_equal(x, 1)
    x <- write_row_category("0300230S", con, paste("INSERT INTO category",
                                        "(id, name, source_id)",
                                        "VALUES",
                                        "($1, $2, $3)"), 0, full)
    expect_equal(x, 0)

  })
})


