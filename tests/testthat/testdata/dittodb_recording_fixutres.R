library(DBI)
library(RPostgres)
library(dplyr)
library(dittodb)
library(SURSfetchR)
library(testthat)

start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))

on.exit(dbDisconnect)
dbSendQuery(con, "set search_path to test_platform")
get_table_id("0300230S", con)
stop_db_capturing()


start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))

on.exit(dbDisconnect)
dbSendQuery(con, "set search_path to test_platform")
SURSfetchR:::get_table_id("x", con)
stop_db_capturing()


start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))

on.exit(dbDisconnect)
dbSendQuery(con, "set search_path to test_platform")
SURSfetchR:::get_unit_id("%", con)
stop_db_capturing()






