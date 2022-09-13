full <- readRDS(test_path("testdata", "full_h.rds"))

dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbSendQuery(con, "set search_path to test_platform")

  test_that("preparing tables", {
    x <- SURSfetchR:::prepare_table_table("1700104S")
    expect_true(all(dim(x) == c(1,6)))
    x <- SURSfetchR:::prepare_category_table("1700104S", full)
    expect_true(all(dim(x) == c(6,3)))
    x <- SURSfetchR:::prepare_category_relationship_table("1700104S", full)
    expect_true(all(dim(x) == c(6,4)))
    x <- SURSfetchR:::prepare_category_table_table("1700104S", full, con)
    expect_true(all(dim(x) == c(1,3)))
    expect_true(x$category_id == 321)
    x <- SURSfetchR:::prepare_table_dimensions_table("1700104S", con)
    expect_true(all(dim(x) == c(3,3)))
    expect_true(x$time[1])
  })
})




