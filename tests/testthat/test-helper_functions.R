dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("expanding level codes", {
    out1 <- expand_to_level_codes("1912002S", NA, con)
    expect_true(nrow(out1) == 14)
    expect_true(ncol(out1) == 3)
    expect_true(all(is.na(out1$unit_id)))
    out2 <- expand_to_series_titles("1912002S", con)
    expect_true(nrow(out2) == 14)
    expect_true(ncol(out2) == 1)
    out3 <- expand_to_level_codes("0300230S", NA, con)
    x <- get_level_text_from_meritve(2, con)
    units <- get_unit_levels_from_meritve(x, con)
    out4 <- add_meritve_level_units(out3, 2, units)
    expect_true(nrow(out4) == 360)
    expect_true(ncol(out4) == 4)
    expect_equal(unique(out4$unit_id), c(1,2,3))
    out5 <- expand_to_level_codes("1700104S", NA, con)
    units <- get_valuenotes_from_px("1700104S", 15, con)
    out6 <- add_valuenotes_level_units(out5, 2, units)
    expect_true(nrow(out6) == 12)
    expect_true(ncol(out6) == 3)
    expect_true(all(unique(out6$unit_id) == c(2,17)))

  })
})



