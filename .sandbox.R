
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
# # set schema search path
# DBI::dbExecute(con, "set search_path to test_platform")

# get list of table codes we want in the database
# not sure when this df was created, it has unused columns, which is whack.
master_list_surs <- readRDS("../mesecni_kazalniki/data/master_list_surs.rds")

# deprecated when str table building was moved to sql.
# insert_table <- readRDS("M:/analysis/SURSfetchR/tests/testthat/testdata/insert_table.rds")

# full hierarchy of matrices and tables, needed when inserting structure tables
# for a new matrix (because of parent fields and stuff)
full<- readRDS("M:/analysis/mesecni_kazalniki/data/full_field_hierarchy.rds")

## deprecated when str table building was moved to sql.
## first option wrote one table, eg 10, second wrote all of them in insert_table for a single matrix.
# system.time(purrr::walk2(insert_table$table[10], insert_table$sql[10], ~
#                            write_multiple_rows(master_list_surs, con, .x, .y, full)))
# system.time(purrr::walk2(insert_table$table, insert_table$sql, ~
#                            write_multiple_rows(data.frame(code = "2771104S"), con, .x, .y, full)))



# logging in as  postgres - required to rebuild from scratch.
con <- dbConnect(RPostgres::Postgres(),
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


# insert table structures for a single matrix
out <- insert_new_table_structures("0714621S", con, full)

# insert table structures for whole list of matrices
system.time(purrr::walk(master_list_surs$code, ~insert_new_table_structures(.x, con, full)))

# insert data for whole list of matrices
system.time(purrr::walk(master_list_surs$code, ~insert_new_data(.x, con)))

# insert table structures for a single matrix
out <- insert_new_data("0714621S", con)

# simple read.px for a single matrix
id <- "1505000S"
url <- paste0("https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/", id, ".px")
l <- pxR::read.px(url,
                  encoding = "CP1250",
                  na.strings = c('"."', '".."', '"..."', '"...."'))



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

code_no <- "0400600S"

insert_data_points <- function(code_no, con){
  df <- prepare_data_table(code_no, con)
  names(df) <- gsub("[^\x01-\x7F]+", "", names(df))
  dbWriteTable(con,
               "new_vintages",
               df,
               temporary = TRUE,
               overwrite = TRUE)

  tbl_id <- SURSfetchR:::get_table_id(code_no, con)
  dim_id <- dbGetQuery(con,
                       sprintf("SELECT id FROM test_platform.table_dimensions where
           table_id = %s and is_time is not true", bit64::as.integer64(tbl_id)))
  dim_id_str <- toString(sprintf("%s", bit64::as.integer64(dim_id$id)))
  tbl_dims <- dbGetQuery(con,
                         sprintf("SELECT replace(dimension, ' ', '.') as dimension
                               FROM test_platform.table_dimensions
                               where id in (%s)
                               order by 1",
                               dim_id_str))
  tbl_dims_str_w_types <- toString(paste(sprintf('"%s"', tbl_dims$dimension), "text"))
  tbl_dims_str <- toString(paste(sprintf('"%s"', tbl_dims$dimension)))
  time <- SURSfetchR:::get_time_dimension(code_no, con)
  interval_id <- SURSfetchR:::get_interval_id(time)

  series_levels_wide <- dbExecute(con,
                                  sprintf("CREATE TEMP TABLE tmp AS
                                    select * from new_vintages
                                    left join
                                    (select *
                                    from crosstab(
                                    'SELECT series_id,  j.dimension, level_value
                                    FROM test_platform.series_levels
                                    left join
                                    (SELECT id, dimension
                                    FROM test_platform.table_dimensions
                                    where id in (%s)) as j
                                    on tab_dim_id = j.id
                                    where tab_dim_id in (%s)
                                    ORDER BY 1,2',
                                    'select distinct dimz.dimension from
                                    (SELECT id, dimension FROM test_platform.table_dimensions
                                     where id in (%s)) as dimz')
                                    as t(series_id int, %s )) i using (%s)
                                    left join
                                    (select distinct on (series_id)
                                    id as vintage_id, series_id from
                                    test_platform.vintage
                                    order by series_id, published) as vinz using (series_id)
                                    ",
                                    dim_id_str,
                                    dim_id_str,
                                    dim_id_str,
                                    gsub("[^\x01-\x7F]+", "",tbl_dims_str_w_types),
                                    gsub("[^\x01-\x7F]+", "",tbl_dims_str)))

  dbExecute(con, sprintf("alter table \"tmp\" add  \"time\" varchar"))
  dbExecute(con, sprintf("alter table \"tmp\" add \"flag\" varchar"))
  dbExecute(con, sprintf("alter table \"tmp\" add \"interval_id\" varchar"))

  dbExecute(con, sprintf("UPDATE \"tmp\" SET
                        \"time\" = split_part(%s, ' ', 1),
                        \"flag\" = substring(%s,
                        (length(split_part(%s,' ',1)))+1,
                        (length(%s)) - (length(split_part(%s,' ',1)))),
                       \"interval_id\" = %s",
                       dbQuoteIdentifier(con,gsub("[^\x01-\x7F]+", "",time)),
                       dbQuoteIdentifier(con,gsub("[^\x01-\x7F]+", "",time)),
                       dbQuoteIdentifier(con,gsub("[^\x01-\x7F]+", "",time)),
                       dbQuoteIdentifier(con,gsub("[^\x01-\x7F]+", "",time)),
                       dbQuoteIdentifier(con,gsub("[^\x01-\x7F]+", "",time)),
                       dbQuoteLiteral(con, interval_id)))

  # change "zacasni podatki" to flag "T"
  dbExecute(con, sprintf("UPDATE \"tmp\" SET
                       flag = 'T' where flag = '(začasni podatki)'"))

  x <- dbExecute(con, sprintf("insert into %s.flag_datapoint
                       select vintage_id, \"time\", flag from
                       tmp where tmp.flag <> ''
                       on conflict do nothing",
                       dbQuoteIdentifier(con, "test_platform")))
  print(paste(x, "new rows intserted into the flag_datapoint table"))

  x <- dbExecute(con, sprintf("insert into %s.period
                       select distinct on (\"time\") \"time\", tmp.interval_id from tmp
                       left join %s.period on \"time\" = id
                       on conflict do nothing",
                       dbQuoteIdentifier(con, "test_platform"),
                       dbQuoteIdentifier(con, "test_platform")))
  print(paste(x, "new rows intserted into the period table"))

  x <- dbExecute(con, sprintf("insert into %s.data_points
                       select vintage_id, time, value from tmp
                       on conflict do nothing",
                       dbQuoteIdentifier(con, "test_platform")))
  print(paste(x, "new rows intserted into the data_points table"))
  dbExecute(con, sprintf("drop table tmp"))
}


tbl(con, "new_vintages") %>%
  collect()

insert_data_points("0400600S", con)


# 0457201S ima zaupno oznako



df <- data.frame(čaj = 1)
dbWriteTable(con,
             "x1",
             df,
             temporary = TRUE,
             overwrite = TRUE)
Encoding(names(df))
Encoding(names(df)) <- "UTF-8"
