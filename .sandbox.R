
# sandboxing db import stuff
devtools::install_github("majazaloznik/SURSfetchR")
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

# insert tbl structures for single table
res <- insert_new_table_structures("0714621S", con)

# insert table structures for whole list of tables.
purrr::walk(master_list_surs$code, ~insert_new_table_structures(.x, con, full))

# insert table structures for whole list of tables.
purrr::walk(master_list_surs$code, ~insert_new_data(.x, con))

out <- insert_new_table_structures("1817902S", con, full)


id <- "0811602S"
url <- paste0("https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/", id, ".px")
l <- pxR::read.px(url,
                  encoding = "CP1250",
                  na.strings = c('"."', '".."', '"..."', '"...."')
)


# dbExecute(con, "CREATE EXTENSION tablefunc;")


code_no <- "2080005S"

df <- prepare_data_table(code_no, con)


write_to_temp_table <- function(con, name, df) {
  dbWriteTable(con,
               name,
               df,
               temporary = TRUE,
               overwrite = TRUE)
  # on.exit(tryCatch(
  #   dbRemoveTable(con, name),
  #   warning = function(w) {
  #     suppressWarnings(dbRemoveTable(con, name, fail_if_missing = FALSE))
  #     if(grepl("Closing open result set", w)) {
  #       NULL
  #     } else {
  #       warning(w)
  #     }
  #   })
  # )
}

colnames(df)[3] <- "CETRTLETJE"
write_to_temp_table(con,
                    "test",
                    df)

dbGetQuery(con, "SELECT * FROM test limit 10")
tbl_id <- dbGetQuery(con, sprintf("SELECT id FROM test_platform.table where code = '%s'"
                     , code_no))
dim_id <- dbGetQuery(con,
           sprintf("SELECT id FROM test_platform.table_dimensions where
           table_id = %s and is_time is not true", bit64::as.integer64(tbl_id$id)))

dim_id_str <- toString(sprintf("%s", bit64::as.integer64(dim_id$id)))


tbl_dims <- dbGetQuery(con,
                       sprintf("SELECT replace(dimension, ' ', '.') as dimension
                               FROM test_platform.table_dimensions
                               where id in (%s)
                               order by 1",
                               dim_id_str))

tbl_dims_str_w_types <- toString(paste(sprintf('"%s"', tbl_dims$dimension), "text"))
tbl_dims_str <- toString(paste(sprintf('"%s"', tbl_dims$dimension)))

# series_levels <- dbGetQuery(con,
#                             sprintf("SELECT series_id, level_value, j.dimension FROM test_platform.series_levels left join
#                                     (SELECT id, dimension FROM test_platform.table_dimensions
#                                      where id in (%s)) as j
#                                     on tab_dim_id = j.id
#                                     where tab_dim_id in (%s) ",
#                                     dim_id_str, dim_id_str))


series_levels_wide <- dbGetQuery(con,
                            sprintf("select * from test
                                    left join
                                    (select *
                                    from crosstab(
                                    'SELECT series_id,  j.dimension, level_value FROM test_platform.series_levels left join
                                    (SELECT id, dimension FROM test_platform.table_dimensions
                                     where id in (%s)) as j
                                    on tab_dim_id = j.id
                                    where tab_dim_id in (%s)
                                    ORDER BY 1,2',
                                    'select distinct j.dimension from
                                    (SELECT id, dimension FROM test_platform.table_dimensions
                                     where id in (%s)) as j')
                                    as t(series_id int, %s )) i using(%s)
                                    ",
                                    dim_id_str, dim_id_str, dim_id_str, tbl_dims_str_w_types,tbl_dims_str ))




