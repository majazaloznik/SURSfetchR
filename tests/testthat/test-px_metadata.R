full <- readRDS(test_path("testdata", "full_h.rds"))

test_that("Metadata is properly parsed", {
  expect_error(get_px_metadata("222"))
  mtdt_tbl <- get_px_metadata("2221702S")
  checkmate::expect_data_frame(mtdt_tbl)
  expect_true(inherits(mtdt_tbl$notes, "json"))
  expect_equal(mtdt_tbl, get_px_metadata("2221702S.px"))
  expect_equal(mtdt_tbl, get_px_metadata("2221702S.PX"))
  checkmate::expect_posixct(mtdt_tbl$updated)
  expect_true(ncol(mtdt_tbl)== 8)
})


test_that("Categories are properly parsed", {
  x <- get_row(12964, full)
  checkmate::expect_data_frame(x)
  expect_true(nrow(x) == 7)
  expect_true(ncol(x) == 4)
  x <- get_row("lkj", full)
  expect_true(nrow(x) == 0)
 expect_error(get_row(123, x))
})

dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("single unit from px metadata", {
  x <- get_single_unit_from_px("0300230S", con)
  expect_true(is.na(x))
  x <- get_single_unit_from_px("0457101S", con)
  expect_true(x == 7)
  })
})

test_that("valuenotes regex works", {
  x <- get_px_metadata("1700104S")$valuenotes[[1]][1]
  out <- get_valuenotes_dimension(names(x))
  expect_equal(out, "EKONOMSKI KAZALNIKI")
  out <- get_valuenotes_dimension("BLA.BLA.BLA.bla")
  expect_equal(out, "BLA BLA BLA")
  out <- get_valuenotes_dimension("Barabe")
  expect_true(length(out) == 0)
  out <- get_valuenotes_level(names(x))
  expect_true(grepl(".zmogljivosti", out))
  out <- get_valuenotes_unit(x, con)
  expect_true(out == 2)
})


dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("extraction from valuenotes works", {
    x <- get_valuenotes_from_px("1700104S", 15, con)
    expect_true(all(dim(x) == c(6,5)))
    expect_true(all(unique(x$unit_id) == c(2, 17)))
  })
})


