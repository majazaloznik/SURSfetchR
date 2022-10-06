
# sandboxing db import stuff
# devtools::install_github("majazaloznik/SURSfetchR")
library(SURSfetchR)
library(DBI)
library(RPostgres)
library(dplyr)
library(dittodb)

# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
#
#
# # set schema search path
# DBI::dbExecute(con, "set search_path to test_platform")

# prepare sql statements for each table.
master_list_surs <- readRDS("../mesecni_kazalniki/data/master_list_surs.rds")
#
# insert_table <- readRDS("M:/analysis/SURSfetchR/tests/testthat/testdata/insert_table.rds")
#
full<- readRDS("M:/analysis/mesecni_kazalniki/data/full_field_hierarchy.rds")
#
# system.time(purrr::walk2(insert_table$table[10], insert_table$sql[10], ~
#                            write_multiple_rows(master_list_surs, con, .x, .y, full)))

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

#

# tbl( con, "series") %>%
#   summarise(n = n())
#
# system.time(purrr::walk2(insert_table$table[10], insert_table$sql[10], ~
#                            write_multiple_rows(data.frame(code = "2771104S"), con, .x, .y, full)))

# rebuild whole db
build_db_tables(con)
# add sql functions to the database
execute_sql_functions_file(con, "inst/sql/insert_functions.sql")






insert_new_table_structures <- function(code_no, con, full) {
  res <- list()
  res[[1]] <- sql_function_call(con,
                    "insert_new_table",
                    as.list(prepare_table_table(code_no)))

  res[[2]] <- sql_function_call(con,
                    "insert_new_category",
                    as.list(prepare_category_table(code_no, full)))

  res[[3]] <- sql_function_call(con,
                    "insert_new_category_relationship",
                    as.list(prepare_category_relationship_table(code_no, full)))

  res[[4]] <- sql_function_call(con,
                    "insert_new_category_table",
                    as.list(prepare_category_table_table(code_no, full, con)))

  res[[5]] <- sql_function_call(con,
                    "insert_new_table_dimensions",
                    as.list(prepare_table_dimensions_table(code_no, con)))

  res[[6]] <- sql_function_call(con,
                    "insert_new_dimension_levels",
                    as.list(prepare_dimension_levels_table(code_no, con)))

  res[[7]] <- sql_function_call(con,
                    "insert_new_unit",
                    as.list(unname(prepare_unit_table(code_no, con))))

  res[[8]] <-  sql_function_call(con,
                    "insert_new_series",
                    as.list(unname(prepare_series_table(code_no, con))))

  res[[9]] <- sql_function_call(con,
                    "insert_new_series_levels",
                    as.list(unname(prepare_series_levels_table(code_no, con))))
  res
}


res <- insert_new_table_structures("0714621S", con, full)

purrr::walk(master_list_surs$code, ~insert_new_table_structures(.x, con, full))
code_no <- "0714621S"

sql_function_call(con,
                  "insert_new_vintage",
                  as.list(unname(prepare_vintage_table("1817902S",con))))


