
# sandboxing db import stuff
# devtools::install_github("majazaloznik/SURSfetchR")
# library(SURSfetchR)
library(DBI)
library(RPostgres)
library(dplyr)
library(dittodb)
library(SURSfetchR)
devtools::install_github("majazaloznik/UMARaccessR")
library(UMARaccessR)
# get list of table codes we want in the database
# not sure when this df was created, it has unused columns, which is whack.
master_list_surs <- readRDS("../mesecni_kazalniki/data/master_list_surs.rds")

# full hierarchy of matrices and tables, needed when inserting structure tables
# for a new matrix (because of parent fields and stuff)
full<- readRDS("M:/analysis/mesecni_kazalniki/data/full_field_hierarchy.rds")

# logging in as  postgres is required to rebuild from scratch. this is not it.
con <- DBI::dbConnect(RPostgres::Postgres(),
                 dbname = "platform",
                 host = "192.168.38.21",
                 port = 5432,
                 user = "majaz",
                 password = Sys.getenv("PG_MZ_PSW"),
                 client_encoding = "utf8")

# set schema search path
DBI::dbExecute(con, "set search_path to platform")

# check you're connected ok and see how many series are in the series table
tbl( con, "series") %>%
  summarise(n = n())

# rebuilding the whole thing from scratch.
# rebuild whole db - runs build_db.sql
build_db_tables(con)
# alternative should work the same:
execute_sql_file(con, "inst/sql/build_db.sql")

## not sure if i still need this, but it was required in setting up,
## so the crosstabs function worked, which is kinda like pivot
# dbExecute(con, "CREATE EXTENSION tablefunc;")
# add sql functions to the database
execute_sql_functions_file(con, "inst/sql/insert_functions.sql")


# # insert table structures for a single matrix
# out <- insert_new_table_structures("0300260S", con, full)
# actuallly use this instead
add_new_table("0301935S", con)

## add that table to the master list
master_list_surs <- rbind(master_list_surs, c("0300260S", NA, NA))
saveRDS(master_list_surs, "../mesecni_kazalniki/data/master_list_surs.rds")

# insert table structures for whole list of matrices
system.time(purrr::walk(master_list_surs$code, ~insert_new_table_structures(.x, con, full)))

# insert data for whole list of matrices
system.time(purrr::walk(master_list_surs$code, ~insert_new_data(.x, con)))

# # insert data  for a single matrix
# debugonce(insert_new_data)
# out <- insert_new_data("0300260S", con)
# actuallly use this instead
add_new_table("1700107S", con)

tbl( con, "tmp") %>%
  collect() -> x


tbl( con, "tmp") %>%
  collect() -> df

tbl(con, "new_vintages") %>%
  collect()


# add a new table
add_new_table("0419001S", con)

# prepare new selection file
df <- UMARaccessR::get_all_series_wtable_names(con)
UMARaccessR::create_selection_excel(df, outfile = "O:\\Avtomatizacija\\data-platform\\seznam_serij")

# add a new table
SURSfetchR::add_new_table("0425002S", con)

# update all SURS tables
tbl( con, "table") %>%
  filter(source_id  == 1) |>
  select(code) |>
  collect() -> df
system.time(purrr::walk(df$code, ~insert_new_data(.x, con)))


#devtools::install_github("majazaloznik/SURSfetchR")
library(dplyr)
library(SURSfetchR)

# logging in as  maintainer
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "platform",
                      host = "localhost",
                      port = 5432,
                      user = "mzaloznik",
                      password = Sys.getenv("PG_local_MAJA_PSW"),
                      client_encoding = "utf8")
# set schema search path
DBI::dbExecute(con, "set search_path to test_platform")

# # # check you're connected ok and see how many series are in the series table
# # tbl( con, "series") %>%
# #   summarise(n = n())
#
# get list of all surs tables
tbl(con, "table") %>%
  filter(source_id  == 1) |>
  select(code) |>
  collect() -> df
#
# update all SURS tables
system.time(purrr::walk(df$code[5:8], ~insert_new_data(.x, con, "test_platform")))

insert_new_data(df$code[[7]], con, schema = "test_platform")


tbl(con, "tmp") %>%
  collect() -> xxx
