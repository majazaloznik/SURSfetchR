library(dittodb)
full <- readRDS(test_path("testdata", "full_h.rds"))

dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("preparing tables", {
    x <- prepare_table_table("1700104S")
    expect_true(all(dim(x) == c(1,6)))
    x <- prepare_category_table("1700104S", full)
    expect_true(all(dim(x) == c(6,3)))
    x <- prepare_category_relationship_table("1700104S", full)
    expect_true(all(dim(x) == c(6,3)))
    x <- prepare_category_table_table("1700104S", full, con)
    expect_true(all(dim(x) == c(1,3)))
    expect_true(x$category_id == 321)
    x <- prepare_table_dimensions_table("1700104S", con)
    expect_true(all(dim(x) == c(3,3)))
    expect_true(x$is_time[1])
    x <- prepare_dimension_levels_table("1700104S", con)
    expect_true(all(dim(x) == c(8,3)))
    expect_true(max(x$level_value) == 6)
    x <- prepare_unit_table("1700104S", con)
    expect_true(all(dim(x) == c(2,1)))
    expect_true(length(unique(x[,1])) == 2)
    x <- prepare_series_table("1700104S", con)
    expect_true(all(dim(x) == c(12,5)))
    expect_true("SURS--1700104S--1--1--Q" %in% x$series_code)
    x <- prepare_series_table("0300230S", con)
    expect_true(all(dim(x) == c(408,5)))
    expect_true("SURS--0300230S--P3_P5--V--N--Q" %in% x$series_code)
    x <- prepare_series_levels_table("1700104S", con)
    expect_true(all(dim(x) == c(24,3)))
    expect_true(length(unique(x$tab_dim_id)) == 2)
  })
})




