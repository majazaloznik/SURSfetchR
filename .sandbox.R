
# sandboxing db import stuff
devtools::install_github("majazaloznik/SURSfetchR")
library(SURSfetchR)
library(DBI)
library(RPostgres)
library(dplyr)
library(dittodb)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))


# set schema search path
DBI::dbExecute(con, "set search_path to test_platform")

# prepare sql statements for each table.
master_list_surs <- readRDS("../mesecni_kazalniki/data/master_list_surs.rds")

insert_table <- readRDS("M:/analysis/SURSfetchR/tests/testthat/testdata/insert_table.rds")

full<- readRDS("M:/analysis/mesecni_kazalniki/data/full_field_hierarchy.rds")

system.time(purrr::walk2(insert_table$table[10], insert_table$sql[10], ~
                           write_multiple_rows(master_list_surs, con, .x, .y, full)))

#
#
#
# code_no <- "0300230S" # meritve, tri enote
# code_no <- "1701106S" # indeks, ena enota
# code_no <- "1700104S" # valuenotes, dve enoti


# rebuild databsae as postgres
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "postgres",
                 password = Sys.getenv("PG_local_PG_PSW"))
# set schema search path
DBI::dbExecute(con, "set search_path to test_platform")

# build_db_tables(con)

tbl( con, "series") %>%
  summarise(n = n())

system.time(purrr::walk2(insert_table$table[10], insert_table$sql[10], ~
                           write_multiple_rows(data.frame(code = "2771104S"), con, .x, .y, full)))




execute_sql_functions_file(con, "inst/sql/insert_functions.sql")


sql_function_call(con,
                  "insert_new_table",
                  as.list(prepare_table_table("0700942S")))


sql_function_call(con,
                  "insert_new_category",
                  as.list(prepare_category_table("0700942S", full)))

