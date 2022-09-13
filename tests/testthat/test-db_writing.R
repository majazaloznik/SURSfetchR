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

    x <- write_row_category_table("1700104S", con, paste("INSERT INTO category_table",
                                                         "(table_id, category_id, source_id)",
                                                         "VALUES",
                                                         "($1, $2, $3)"), 0, full)
    expect_equal(x, 1)

    x <- write_row_table_dimensions("1700104S", con, paste("INSERT INTO table_dimensions",
                                                           "(table_id, dimension, time)",
                                                           "VALUES",
                                                           "($1, $2, $3)"), 0)
    expect_equal(x, 3)
    x <- write_row_dimension_levels("1700104S", con, paste("INSERT INTO dimension_levels",
                                                          "(tab_dim_id, level_value, level_text)",
                                                          "VALUES",
                                                          "($1, $2, $3)"), 0)
    expect_equal(x, 8)

  })
})


