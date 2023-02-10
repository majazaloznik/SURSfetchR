
# sandboxing db import stuff
# devtools::install_github("majazaloznik/SURSfetchR")
# library(SURSfetchR)
library(DBI)
library(RPostgres)
library(dplyr)
library(dittodb)
library(SURSfetchR)

# get list of table codes we want in the database
# not sure when this df was created, it has unused columns, which is whack.
master_list_surs <- readRDS("../mesecni_kazalniki/data/master_list_surs.rds")

# full hierarchy of matrices and tables, needed when inserting structure tables
# for a new matrix (because of parent fields and stuff)
full<- readRDS("M:/analysis/mesecni_kazalniki/data/full_field_hierarchy.rds")

# logging in as  postgres - required to rebuild from scratch.
con <- DBI::dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "postgres",
                 password = Sys.getenv("PG_local_PG_PSW"),
                 client_encoding = "utf8")
# set schema search path
DBI::dbExecute(con, "set search_path to test_platform")

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
# out <- insert_new_table_structures("1701106S", con, full)

# insert table structures for whole list of matrices
system.time(purrr::walk(master_list_surs$code, ~insert_new_table_structures(.x, con, full)))

# insert data for whole list of matrices
system.time(purrr::walk(master_list_surs$code, ~insert_new_data(.x, con)))

# # insert data  for a single matrix
# debugonce(insert_new_data)
out <- insert_new_data("1701102S", con)


tbl( con, "tmp") %>%
  collect() -> df

tbl(con, "new_vintages") %>%
  collect()

insert_data_points("0400600S", con)

code_no <- "1700104S"

tbl(con, "vint")

library("UMARvisualisR")
library(UMARaccessR)

univariate_line_pipeline(6626, rolling = TRUE, interval = "M", unit = "%", con = con)
univariate_line_pipeline(6627, rolling = TRUE, interval = "M", unit = "%", con = con)
univariate_line_pipeline(6625, rolling = TRUE, interval = "M", unit = "%", con = con)

SURSfetchR:::get_table_id("1700104S", con)


get_table_id <- function(code_no, con) {
  dplyr::tbl(con, "table") %>%
    dplyr::filter(code == code_no) %>%
    dplyr::pull(id)
}

get_table_id("1700104S", con)


tbl( con, "series") %>%
left_join(tbl( con, "table"), by = c("table_id"= "id")) %>%
  select(-url, -source_id, -description, -notes) %>%
  left_join(tbl(con, "unit"), by = c("unit_id"="id")) %>%
  rename(unit = name.y,
         table_code = code.y,
         series_code = code.x,
         table_name = name.x,
         series_name = name_long) %>%
  select(table_code, table_name,
         series_code, series_name,
         unit, interval_id) %>%
  arrange(table_code, series_code) %>%
  collect() -> series_df

nejmz <- c(names(series_df), "chart_no", "rolling_average_periods",
  "rolling_average_alignment", 	"year_on_year", "xmin",	"xmax")

selection_df <- setNames(data.frame(matrix(ncol = length(nejmz), nrow = 0)), nejmz)


wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "series")
openxlsx::addWorksheet(wb, "selection")

openxlsx::writeData(wb, "series", series_df, startRow = 1, startCol = 1)
openxlsx::writeData(wb, "selection", selection_df, startRow = 1, startCol = 1)
openxlsx::freezePane(wb,"series", firstActiveRow = 2)
openxlsx::freezePane(wb,"selection", firstActiveRow = 2)
openxlsx::saveWorkbook(wb, file = "../mesecni_kazalniki/data/db_series.xlsx", overwrite = TRUE)

