

# sandboxing db import stuff

library(SURSfetchR)
master_list_surs <- readRDS("../mesecni_kazalniki/data/master_list_surs.rds")

library(DBI)
library(RPostgres)
library(dplyr)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))


# set schema search path
dbSendQuery(con, "set search_path to test_platform")

# prepare sql statements for each table.
insert_table <- data.frame(table = character(),
                           sql = character())
insert_table <- bind_rows(insert_table,
                          c(table = "table",
                            sql = paste("INSERT INTO \"table\"",
                                        "(code, name, source_id, url, notes)",
                                        "VALUES",
                                        "($1, $2, $3, $4, $5)")))

insert_table <- bind_rows(insert_table,
                          c(table = "category",
                            sql = paste("INSERT INTO category",
                                        "(id, name, source_id)",
                                        "VALUES",
                                        "($1, $2, $3)")))

insert_table <- bind_rows(insert_table,
                          c(table = "category_relationship",
                            sql = paste("INSERT INTO category_relationship",
                                        "(category_id, parent_id, source_id)",
                                        "VALUES",
                                        "($1, $2, $3)")))

insert_table <- bind_rows(insert_table,
                          c(table = "category_table",
                            sql = paste("INSERT INTO category_table",
                                        "(table_id, category_id, source_id)",
                                        "VALUES",
                                        "($1, $2, $3)")))

insert_table <- bind_rows(insert_table,
                          c(table = "table_dimensions",
                            sql = paste("INSERT INTO table_dimensions",
                                        "(table_id, dimension, time)",
                                        "VALUES",
                                        "($1, $2, $3)")))


insert_table <- bind_rows(insert_table,
                          c(table = "dimension_levels",
                            sql = paste("INSERT INTO dimension_levels",
                                        "(tab_dim_id, level_value, level_text)",
                                        "VALUES",
                                        "($1, $2, $3)")))
insert_table <- bind_rows(insert_table,
                          c(table = "unit",
                            sql =  paste("INSERT INTO unit",
                                         "(name)",
                                         "VALUES",
                                         "($1)")))

insert_table <- bind_rows(insert_table,
                          c(table = "series",
                            sql =  paste("INSERT INTO series",
                                         "(table_id, name_long,  code, interval_id, unit_id)",
                                         "VALUES",
                                         "($1, $2, $3, $4, $5)")))

insert_table <- bind_rows(insert_table,
                          c(table = "series_levels",
                            sql =  paste("INSERT INTO series_levels",
                                         "(series_id, tab_dim_id,  level_value)",
                                         "VALUES",
                                         "($1, $2, $3)")))

full<- readRDS("M:/analysis/mesecni_kazalniki/data/full_field_hierarchy.rds")

code_no <- "0300230S" # meritve, tri enote
code_no <- "1701106S" # indeks, ena enota
code_no <- "1700104S"

system.time(purrr::walk2(insert_table$table, insert_table$sql, ~
                           write_multiple_rows(master_list_surs[1,], con, .x, .y, full)))



sql_statement <- paste("INSERT INTO series",
      "(table_id, name_long,  code, interval_id, unit_id)",
      "VALUES",
      "($1, $2, $3, $4, $5)")
