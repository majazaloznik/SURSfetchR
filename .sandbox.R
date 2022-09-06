

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
    dplyr::filter(table_id == tbl_id,
                  time != TRUE) %>%
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
    dplyr::mutate(series_code = paste0("SURS--", code_no, "--", series_code, "--",interval_id)) %>%
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

url <- paste0("https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/", code_no, ".px")
l <- pxR::read.px(url,
                  encoding = "CP1250",
                  na.strings = c('"."', '".."', '"..."', '"...."')
)


id <- "1700104S"
get_px_metadata <- function(id) {
  checkmate::qassert(id, "S[5,11]")
  id <- sub(".PX$", "", id)
  id <- sub(".px$", "", id)
  url <- paste0("https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/", id, ".px")
  l <- pxR::read.px(url,
                    encoding = "CP1250",
                    na.strings = c('"."', '".."', '"..."', '"...."')
  )
  df <- data.frame(code = unlist(l$MATRIX),
                   name = unlist(l$DESCRIPTION),
                   created = as.POSIXct(l$CREATION.DATE[[1]],format="%Y%m%d %H:%M",tz=Sys.timezone()),
                   units = l$UNITS[[1]],
                   notes = I(list(c(l$NOTE, l$NOTEX))),
                   valuenotes =I(list(l$VALUENOTE))) %>%
    dplyr::mutate(notes = jsonlite::toJSON(notes),
                  source = 1,
                  url = paste0("https://pxweb.stat.si/SiStatData/api/v1/sl/Data/", code, ".px"))
  df
}



get_valuenotes_dimension <- function(x){
  x <- regmatches(x, regexpr("[A-Z.]+(?=\\.)", x, perl = TRUE))
  gsub( "\\.", " ", x)
}

get_valuenotes_level <- function(x){
  y <- gsub( "\\.", " ", x)
  gsub(paste0(get_valuenotes_dimension(x), " "), "", y)
}

get_valuenotes_unit <- function(x){
  y <- regmatches(x, regexpr("(?<=Enota: ).+", x, perl = TRUE))
  y <- gsub( "\\.", "", y)
  unit_name <- gsub("\" \"", "", y)
  as.numeric(get_unit_id(unit_name))
}

get_tab_dim_id <- function(tbl_id, dim_name) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  dimension == dim_name) %>%
    dplyr::pull(id)
}

get_level_value <- function(tbl_dm_id, lvl_text) {
  dplyr::tbl(con, "dimension_levels") %>%
    dplyr::filter(tab_dim_id == tbl_dm_id,
                  level_text == lvl_text) %>%
    dplyr::pull(level_value)
}


x <- get_px_metadata(id)$valuenotes

xx <- purrr::map(x, names)[[1]]



df <- map_dfr(xx, ~ c(dim_name = get_valuenotes_dimension(.),
                      level_text = get_valuenotes_level(.))) %>%
  rowwise() %>%
  mutate(tab_dim_id = get_tab_dim_id(tbl_id, dim_name),
         level_value = get_level_value(tab_dim_id, level_text)) %>%
  ungroup() %>%
  mutate(unit_id = purrr::map_dbl(x[[1]], get_valuenotes_unit))

