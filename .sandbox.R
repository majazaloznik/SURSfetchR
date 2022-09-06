

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
                                        "(code, name, source_id, url, description, notes)",
                                        "VALUES",
                                        "($1, $2, $3, $4, $5, $6)")))

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
sql_statement <- paste("INSERT INTO series",
                       "(table_id, name_long,  code, interval_id, unit_id)",
                       "VALUES",
                       "($1, $2, $3, $4, $5)")

write_row_series(code_no, dbseries = NULL,
                 con, sql_statement, counter = 0)


write_row_series <- function(code_no, dbseries, con, sql_statement, counter, ...) {

  get_table_id(code_no) -> tbl_id

  interval_lookupV <- setNames(c("Q", "M", "Y"), c("\\u010cETRTLETJE", "MESEC", "LETO"))
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::filter(time) %>%
    dplyr::collect() %>%
    dplyr::pull(dimension) -> interval_text
  interval_id <- ifelse(stringi::stri_escape_unicode(interval_text) %in% names(interval_lookupV),
                   getElement(interval_lookupV, stringi::stri_escape_unicode(interval_text)), NA)

  units_from_px <- unlist(strsplit(get_px_metadata(code_no)$units, ", "))
  if(length(units_from_px)==1) {
    unit_id <- get_unit_id(units_from_px)} else {
      unit_id <- NA}

  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    filter(dimension == "MERITVE") %>%
    pull(id) -> meritve_dim_id

  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::mutate(poz = dplyr::row_number()) %>%
    filter(dimension == "MERITVE") %>%
    pull(poz) -> meritve_dim_no

  if(length(meritve_dim_id) == 1) {# if MERITVE EXIST
    dplyr::tbl(con, "dimension_levels") %>%
      dplyr::filter(tab_dim_id == meritve_dim_id) %>%
      dplyr::collect() %>%
      dplyr::mutate(unit = regmatches(level_text, regexpr("(?<=[(]{1})([^)]+)(?=[)]{1}$)",
                                                          level_text, perl = TRUE))) %>%
      dplyr::select(-level_text) %>%
      rowwise() %>%
      dplyr::mutate(unit_id = get_unit_id(unit)) -> units_by_meritve_levels
  }

  get_table_levels(code_no) %>%
    dplyr::filter(!time) %>%
    dplyr::pull(levels) %>%
    purrr::map("values") %>%
    expand.grid(stringsAsFactors = FALSE) %>%
    mutate(unit_id = unit_id) -> expanded_level_codes

  if(length(meritve_dim_id) == 1 & all(is.na(tmp2$level_value))) {
    expanded_level_codes %>%
      select(-unit_id) %>%
      rename("level_value" := !!(paste0("Var", meritve_dim_no))) %>%
      left_join(units_by_meritve_levels, by = c("level_value" = "level_value")) %>%
      rename(!!(paste0("Var", meritve_dim_no)) := "level_value") %>%
      select(-tab_dim_id, -unit) -> expanded_level_codes
  }

  expanded_level_codes %>%
    tidyr::unite("series_code", dplyr::starts_with("Var"), sep = "--") %>%
    dplyr::mutate(series_code = paste0("SURS--", code_no, "--", series_code, "--",int_id)) %>%
    cbind(get_table_levels(code_no) %>%
            dplyr::filter(!time) %>%
            dplyr::pull(levels) %>%
            purrr::map("valueTexts") %>%
            expand.grid() %>%
            tidyr::unite("series_title", dplyr::everything(), sep = " - ")) -> tmp

  counter_i = 0
  for (i in seq_len(nrow(tmp))){
    tryCatch({
      dbExecute(con, sql_statement, list(tbl_id,
                                         tmp[i,]$series_title,
                                         tmp[i,]$series_code,
                                         interval_id,
                                         tmp[i,]$unit_id))
      counter_i <- counter_i + 1
      counter <- counter + 1
    },
    error = function(cnd) {
      print(cnd)
    }
    )
  }
  message(paste(counter_i, "new series inserted from matrix ", code_no))
  return(counter)
}

