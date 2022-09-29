#' Helper functions to get stuff out of db tables
#'
#' Group of functions that lookup stuff in relevant tables on the `con` connection
#' to get correct table id from px code. Also make sure the `search_path` is set to
#' the correct scheme.
#'
#' NB: there is an issue with BIGINT datatype being read by dittodb in R, that's
#' why those have an `as.numeric()` added at the end, because there is no native
#' type in R or sth.
#'
#' @param code_no px. code e.g. 0300230S
#' @param con connection to postgres database
#' @param unit char text of unit label
#' @param tbl_id numeric table id from table table
#' @param dim_name character dimension name
#' @param tbl_dm_id numeric table-dimension id from that same table
#' @param lvl_text character text of level label
#' @param meritve_dim_id numeric dimension id of MERITVE dim
#' @rdname get_stuff
#' @return numeric code from the table table
#' @keywords internal
get_table_id <- function(code_no, con) {
  dplyr::tbl(con, "table") %>%
    dplyr::filter(code == code_no) %>%
    dplyr::mutate(id = as.numeric(id)) %>%
    dplyr::pull(id)
}

#' Helper fun to get correct code from unit text
#'
#' @rdname get_stuff
#' @return numeric code from db table unit
#' @keywords internal
get_unit_id <- function(unit, con){
  unit <- tolower(unit)
  dplyr::tbl(con, "unit") %>%
    dplyr::filter(name == unit) %>%
    dplyr::pull(id)
}

#' @rdname get_stuff

#' @return numeric code of table-dimension id
#' @keywords internal
get_tab_dim_id <- function(tbl_id, dim_name, con) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  dimension == dim_name) %>%
    dplyr::mutate(id = as.numeric(id)) %>%
    dplyr::pull(id)
}


#' @rdname get_stuff
#' @return character code from level value code
#' @keywords internal
get_level_value <- function(tbl_dm_id, lvl_text, con) {
  dplyr::tbl(con, "dimension_levels") %>%
    dplyr::filter(tab_dim_id == tbl_dm_id,
                  level_text == lvl_text) %>%
    dplyr::pull(level_value)
}


#' @rdname get_stuff
#' @return character text of time dimension from table code
#' @keywords internal
get_time_dimension <- function(code_no, con) {
  tbl_id <- get_table_id(code_no, con)
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::filter(time) %>%
    dplyr::pull(dimension)
}

#' @rdname get_stuff
#' @return numeric id of MERITVE dimension if it exists, else integer64(0)
#' @keywords internal
get_meritve_id <- function(tbl_id, con) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::filter(dimension == "MERITVE") %>%
    dplyr::mutate(id = as.numeric(id)) %>%
    dplyr::pull(id)
}

#' @rdname get_stuff
#' @return numeric position of MERITVE dimension if it exists, else integer64(0)
#' @keywords internal
get_meritve_no <-function(tbl_id, con) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  time != TRUE) %>%
    dplyr::mutate(poz = dplyr::row_number()) %>%
    dplyr::filter(dimension == "MERITVE") %>%
    dplyr::mutate(poz = as.numeric(poz)) %>%
    dplyr::pull(poz)
}

#' @rdname get_stuff
#' @return tibble with 4 cols including `level_value` and `unit_id`
#' @keywords internal
get_level_text_from_meritve <- function(meritve_dim_id, con){
  dplyr::tbl(con, "dimension_levels") %>%
    dplyr::filter(tab_dim_id == meritve_dim_id) %>%
    dplyr::collect()
}

#' @rdname get_stuff
#' @return tibble with 4 cols including `level_value` and `unit_id`
#' @keywords internal
get_unit_levels_from_meritve <- function(meritve_level_text, con){
  meritve_level_text %>%
    dplyr::mutate(unit = regmatches(level_text, regexpr("(?<=[(]{1})([^)]+)(?=[)]{1}$)",
                                                        level_text, perl = TRUE))) %>%
    dplyr::select(-level_text) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(unit_id = get_unit_id(unit, con))
}

#' @rdname get_stuff
#' @return numeric id of valuenotes dimension if it exists, else integer64(0)
#' @keywords internal
get_valuenotes_id <- function(tbl_id, dim_name, con) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::filter(dimension == dim_name) %>%
    dplyr::mutate(id = as.numeric(id)) %>%
    dplyr::pull(id)
}

#' @rdname get_stuff
#' @return numeric position of valuenotes dimension if it exists, else integer64(0)
#' @keywords internal
get_valuenotes_no <-function(tbl_id, dim_name, con) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  time != TRUE) %>%
    dplyr::mutate(poz = dplyr::row_number()) %>%
    dplyr::filter(dimension == dim_name) %>%
    dplyr::mutate(poz = as.numeric(poz)) %>%
    dplyr::pull(poz)
}


#' Helper to get interval id from lookup vector
#'
#' This is not a db reading function, but it kinda fits with them, simply
#' because i'm using a lookup vector inside the funciton instead of creating
#' another table in the database to get the interval id.
#'
#' @param interval_text text of the time interval name
#'
#' @return character code of interval id
#' @keywords internal
get_interval_id <- function(interval_text) {
  interval_lookupV <- setNames(c("Q", "M", "A", "Q", "Q"),
                               c("\\u010cETRTLETJE", "MESEC", "LETO", "?ETRTLETJE",
                                 "\\u00c4\\u015aETRTLETJE"))
  interval_id <- ifelse(stringi::stri_escape_unicode(interval_text) %in% names(interval_lookupV),
                        getElement(interval_lookupV, stringi::stri_escape_unicode(interval_text)), NA)
  interval_id
}

#' Helper fun to get series id given a series code
#'
#' @rdname get_stuff
#' @return numeric code from db table series
#' @keywords internal
get_series_id <- function(series_code, con){
  dplyr::tbl(con, "series") %>%
    dplyr::filter(code %in% series_code) %>%
    dplyr::pull(id)
}

#' Helper fun to get series id given a table id
#'
#' @rdname get_stuff
#' @return numeric code from db table series
#' @keywords internal
get_series_id_from_table <- function(tbl_id, con){
  dplyr::tbl(con, "series") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::pull(id)
}

#' Helper fun to get most recent publication date for given table
#'
#' @rdname get_stuff
#' @return numeric code from db table series
#' @keywords internal
get_last_publication_date <- function(tbl_id, con){
  get_series_id_from_table(tbl_id, con) -> series_ids
  dplyr::tbl(con, "vintage") %>%
    dplyr::filter(series_id %in% series_ids) %>%
    dplyr::distinct(published) %>%
    dplyr::pull(published)
}
