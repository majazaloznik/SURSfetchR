
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

# insert data for whole list of tables.
purrr::walk(master_list_surs$code, ~insert_new_data(.x, con))

out <- insert_new_table_structures("1505000S", con, full)


id <- "1505000S"
url <- paste0("https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/", id, ".px")
l <- pxR::read.px(url,
                  encoding = "CP1250",
                  na.strings = c('"."', '".."', '"..."', '"...."')
)


# # need this for the crosstabs function which is kinda like pivot_wide
# dbExecute(con, "CREATE EXTENSION tablefunc;")





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

#colnames(df)[3] <- "CETRTLETJE"
code_no <- "2080005S"

df <- prepare_data_table(code_no, con)

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
dbExecute(con, "DROP TABLE tmp")

series_levels_wide <- dbExecute(con,
                            sprintf("CREATE TEMP TABLE tmp AS
                                    select * from test
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
                                    tbl_dims_str_w_types,
                                    tbl_dims_str ))

tmp <- dbGetQuery(con, "SELECT * FROM tmp")
time <- SURSfetchR:::get_time_dimension(code_no, con)



interval_id <- SURSfetchR:::get_interval_id(time)

dbExecute(con, sprintf("alter table \"tmp\" add  \"time\" varchar"))
dbExecute(con, sprintf("alter table \"tmp\" add \"flag\" varchar"))
dbExecute(con, sprintf("alter table \"tmp\" add \"interval_id\" varchar"))
dbExecute(con, sprintf("alter table \"tmp\" add \"previosus_vintage\" varchar"))

dbExecute(con, sprintf("UPDATE \"tmp\" SET
                        \"time\" = split_part(%s, ' ', 1),
                        \"flag\" = substring(\"MESEC\",
                        (length(split_part(\"MESEC\",' ',1)))+1,
                        (length(\"MESEC\")) - (length(split_part(\"MESEC\",' ',1)))),
                       \"interval_id\" = %s",
                        dbQuoteIdentifier(con,time),
                       dbQuoteLiteral(con, interval_id)))

dbExecute(con, sprintf("insert into %s.period
                       select distinct on (\"time\") \"time\", tmp.interval_id from tmp
                       left join %s.period on \"time\" = id
                       on conflict do nothing",
                       dbQuoteIdentifier(con, "test_platform"),
                       dbQuoteIdentifier(con, "test_platform")))

dbExecute(con, sprintf("insert into %s.data_points
                       select vintage_id, time, value from tmp
                       on conflict do nothing",
                       dbQuoteIdentifier(con, "test_platform")))

# test data
dbExecute(con, sprintf("insert into %s.vintage (series_id, published)
                       values (1862, '2022-06-10 10:20:00'),
                       (1862,  '2022-06-10 10:22:00')",
                       dbQuoteIdentifier(con, "test_platform")))

dbExecute(con, sprintf("insert into %s.vintage (series_id, published)
                       values (1862, '2022-06-10 10:20:00'),
                       (1862,  '2022-06-10 10:22:00')",
                       dbQuoteIdentifier(con, "test_platform")))


# # don't know what this is, not working rn.
# dbExecute(con, sprintf("delete from %s.data_points
#                        where vintage_id in (select vintage_id, time, value from tmp)",
#                        dbQuoteIdentifier(con, "test_platform")))


x <- dbGetQuery(con, sprintf("SELECT * FROM vintage
                        where series_id in (select series_id from \"tmp\")"))

tmp <- dbGetQuery(con, sprintf("SELECT * FROM tmp"))

# # get last two vintages for each series in the table
x <- dbGetQuery(con, sprintf("SELECT v.* from (select distinct series_id from tmp) t
                             cross join lateral (
                             select * from vintage
                             where series_id = t.series_id
                             order by published desc
                             limit 2
                             ) v;"))

# # get list of new vintages in the table
# y <- dbGetQuery(con, sprintf("SELECT t.* from (select distinct tmp.vintage_id from tmp) t
#                              left join (
#                              select * from vintage) v on t.vintage_id = v.id"))


# get previous vintage of any new vintage if exists.
xy <- dbGetQuery(con, sprintf("SELECT v.* from (select distinct series_id from tmp) t
                             cross join lateral (
                             select * from vintage
                             where series_id = t.series_id
                             order by published desc
                             limit 2
                             ) v
                             left join
                             (SELECT t.* from (select distinct tmp.vintage_id from tmp) t
                             left join (
                             select * from vintage) v on t.vintage_id = v.id) b
                             on v.id = b.vintage_id where b.vintage_id is null"))





dbWriteTable(con,
             "tmp_data_points",
             df)


dbWriteTable(con,
             "tmp_data_points",
             df,
             overwrite = TRUE)

execute_sql_functions_file(con, "inst/sql/prepare_data_points.sql")

sql_function_call(con,
                  "prepare_data_points",
                  args = list("1817902S"),
                  schema = "test_platform")


SURSfetchR:::get_unit_levels_from_meritve(level_text_from_meritve, con)
