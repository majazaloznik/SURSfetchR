full <- readRDS(test_path("testdata", "full_h.rds"))
insert_table <- readRDS(test_path("testdata", "insert_table.rds"))

dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

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
    x <- write_row_category_relationship("1700104S", con, paste("INSERT INTO category_relationship",
                                                                "(category_id, parent_id, source_id)",
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
    x <- write_row_unit("1700104S", con, paste("INSERT INTO unit",
                                               "(name)",
                                               "VALUES",
                                               "($1)"), 0)
    expect_equal(x, 2)
    x <- write_row_series("1700104S", con,  paste("INSERT INTO series",
                                                  "(table_id, name_long,  code, interval_id, unit_id)",
                                                  "VALUES",
                                                  "($1, $2, $3, $4, $5)"), 0)
    expect_equal(x, 12)
    x <- write_row_series_levels("1700104S", con, paste("INSERT INTO series_levels",
                                                 "(series_id, tab_dim_id,  level_value)",
                                                 "VALUES",
                                                 "($1, $2, $3)"), 0)
    expect_equal(x, 24)

    x <- purrr::walk2(insert_table$table, insert_table$sql, ~
                        write_multiple_rows(data.frame(code = "1700104S"),
                                            con, .x, .y, full))
    expect_equal(length(x), 9)
    expect_true(all(c("table", "category", "category_relationship", "category_table", "table_dimensions",
      "dimension_levels",  "unit" , "series", "series_levels") %in% x))
  })
})


