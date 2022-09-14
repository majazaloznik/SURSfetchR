# # recording fixtures that are saved in the testthat/sandbox folder for mock testing
# # reading (and writing?) to the database - without which i couldn't write a bunch
# # of the other unit tests.
#
# # This code was run once and is here for archival purposes.
#
library(DBI)
library(RPostgres)
library(dplyr)
library(dittodb)
library(SURSfetchR)
library(testthat)
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_table_id("0300230S", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_table_id("1700104S", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_table_id("x", con)
# stop_db_capturing()
#
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_unit_id("%", con)
# stop_db_capturing()
#
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_unit_id("%", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_tab_dim_id(2, "MERITVE", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_level_value(2, "Tekoče cene (mio EUR)", con)
# stop_db_capturing()
#
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_time_dimension("0300230S", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_time_dimension("1700104S", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_meritve_id(2, con)
# stop_db_capturing()

# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_meritve_id(1, con)
# stop_db_capturing()

# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_meritve_id(15, con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_meritve_no(2, con)
#
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_meritve_no(15, con)
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_meritve_no(15, con)
#
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_level_text_from_meritve(2, con)
# stop_db_capturing()

# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_level_text_from_meritve(1, con)
# stop_db_capturing()

# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# level_text <- SURSfetchR:::get_level_text_from_meritve(2, con)
# SURSfetchR:::get_unit_levels_from_meritve(level_text[1,], con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_unit_id("mio EUR", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_unit_id("indeks", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_unit_id("ravnotežje v odstotnih točkah", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_unit_id("odstotne točke", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_valuenotes_id(14, "EKONOMSKI KAZALNIK", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_valuenotes_id(15, "EKONOMSKI KAZALNIKI", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_valuenotes_no(14, "EKONOMSKI KAZALNIK", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_valuenotes_no(15, "EKONOMSKI KAZALNIKI", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_tab_dim_id(15, "EKONOMSKI KAZALNIKI", con)
# stop_db_capturing()
#
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_level_value(48, "Izkoriščdenost proizvodnih zmogljivosti", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_level_value(48, "Ustreznost proizvodnih zmogljivosti", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_level_value(48, "Konkurenčni položaj na domačem trgu", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_level_value(48, "Konkurenčni položaj na trgih držav EU", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_level_value(48, "Konkurenčni položaj na trgih zunaj EU", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
# SURSfetchR:::get_level_value(48, "Obseg novih naročil", con)
# stop_db_capturing()
#
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
#
# x <- SURSfetchR:::write_row_table("0300230S", con, paste("INSERT INTO \"table\"",
#                                         "(code, name, source_id, url, notes)",
#                                         "VALUES",
#                                         "($1, $2, $3, $4, $5)"), 0)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
#
# dplyr::tbl(con, "table_dimensions") %>%
#   dplyr::filter(table_id == 15) %>%
#   dplyr::filter(time == FALSE) %>%
#   dplyr::select(dimension, id) %>%
#   dplyr::collect()
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
# on.exit(dbDisconnect)
# dbExecute(con, "set search_path to test_platform")
#
# prepare_series_levels_table("1700104S", con)
# stop_db_capturing()

start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))

on.exit(dbDisconnect)
dbExecute(con, "set search_path to test_platform")
full <- readRDS(test_path("testdata", "full_h.rds"))
insert_table <- readRDS(test_path("testdata", "insert_table.rds"))
x <- purrr::walk2(insert_table$table, insert_table$sql, ~
                    write_multiple_rows(data.frame(code = "1700104S"),
                                        con, .x, .y, full))
stop_db_capturing()
