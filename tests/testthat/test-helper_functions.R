library(dittodb)
dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("expanding level codes", {
    out1 <- expand_to_level_codes("1912002S")
    expect_true(nrow(out1) == 16)
    expect_true(ncol(out1) == 2)
    expect_true(all(is.na(out1$unit_id)))
    out2 <- expand_to_series_titles("1912002S")
    expect_true(nrow(out2) == 16)
    expect_true(ncol(out2) == 1)
    out3 <- SURSfetchR:::expand_to_level_codes("0300230S") %>%
      dplyr::mutate(unit_id = NA)
    x <- SURSfetchR:::get_level_text_from_meritve(2, con)
    print(x)
    units <- SURSfetchR:::get_unit_levels_from_meritve(x, con)
    print(units)
    out4 <- SURSfetchR:::add_meritve_level_units(out3, 2, units)
    expect_true(nrow(out4) == 408)
    expect_true(ncol(out4) == 5)
    print(out4$unit_id)
    expect_equal(unique(out4$unit_id), c(2,3, 4))
    out5 <- expand_to_level_codes("1700104S") %>%
      dplyr::mutate(unit_id = NA)
    units <- get_valuenotes_from_px("1700104S", 15, con)
    out6 <- add_valuenotes_level_units(out5, 2, units)
    expect_true(nrow(out6) == 12)
    expect_true(ncol(out6) == 3)
    expect_equal(unique(out6$unit_id) , c(3,18))
    codes <- list(A = c(1, 2))
    labels <- list(A =c( "lejbl", "bla"))
    df <- data.frame(A = "bla", value = 23, MESEC = "2022M02")
    out7 <- recode_labels(1, codes, labels, df)
    expect_equal(out7$A, 2)
  })
})



