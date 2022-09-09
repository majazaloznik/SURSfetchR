dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbSendQuery(con, "set search_path to test_platform")

  test_that("mock tests for get_table", {
    out <- get_table_id("0300230S", con)
    expect_true(length(out) == 1)
    expect_true(out == 1)
    expect_true(length(get_table_id("x", con)) == 0)
  })

  test_that("mock tests for get_unit", {
    expect_true(length(get_unit_id("%", con)) == 1)
    expect_true(get_unit_id("%", con) == 2)
  })

  test_that("mock tests for get_tab_dim_id", {
    out <- get_tab_dim_id(2, "MERITVE", con)
    expect_true(length(out) == 1)
    expect_true(out == 6)
  })

  test_that("mock tests for get_level_value", {
    out <-   get_level_value(2, "Tekoče cene (mio EUR)", con)
    expect_true(length(out) == 1)
    expect_true(out == "V")
  })

  test_that("mock tests for get_interval?id", {
    out <- get_interval_id("0300230S", con)
    expect_true(length(out) == 1)
    expect_true(out == "Q")
  })

  test_that("mock tests for get_meritve_id", {
    out <-   get_meritve_id(2, con)
    expect_true(length(out) == 1)
    expect_true(out == 6)
  })

  test_that("mock tests for get_meritve_no", {
    out <-   get_meritve_no(2, con)
    expect_true(length(out) == 1)
    expect_true(out == 2)
  })

  test_that("mock tests for get_meritve_no", {
    out <- get_unit_levels_from_meritve(2, con)
    expect_true(all(dim(out) == c(6,4)))
  })

  test_that("mock tests for get_valuenotes_id", {
    out <- get_valuenotes_id(14, "EKONOMSKI KAZALNIK", con)
    expect_true(length(out) == 1)
    expect_true(out == 45)
  })

  test_that("mock tests for get_valuenotes_no", {
    out <- get_valuenotes_no(14, "EKONOMSKI KAZALNIK", con)
    expect_true(length(out) == 1)
    expect_true(out == 2)
  })

  dbDisconnect(con)
})
