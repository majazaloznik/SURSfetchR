#' Helper functions to get stuff out of db tables
#'
#' Group of functions that lookup stuff in relevant tables on the `con` connection
#' to get correct table id from px code. Also make sure the `search_path` is set to
#' the correct scheme.
#'
#' @param code_no px. code e.g. 0300230S
#' @param unit char text of unit label
#' @param tbl_id numeric table id from table table
#' @param dim_name character dimension name
#' @param tbl_dm_id numeric table-dimension id from that same table
#' @param lvl_text character text of level label
#' @param meritve_dim_id numeric dimension id of MERITVE dim
#' @rdname get_stuff
#' @return numeric code from the table table
#' @keywords internal
get_table_id <- function(code_no) {
  dplyr::tbl(con, "table") %>%
    dplyr::filter(code == code_no) %>%
    dplyr::pull(id)
}

#' Helper fun to get correct code from unit text
#'
#' @rdname get_stuff
#' @return numeric code from db table unit
#' @keywords internal
get_unit_id <- function(unit){
  unit <- tolower(unit)
  dplyr::tbl(con, "unit") %>%
    dplyr::filter(name == unit) %>%
    dplyr::pull(id)
}

#' @rdname get_stuff

#' @return numeric code of table-dimension id
#' @keywords internal
get_tab_dim_id <- function(tbl_id, dim_name) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  dimension == dim_name) %>%
    dplyr::pull(id)
}


#' @rdname get_stuff
#' @return character code from level value code
#' @keywords internal
get_level_value <- function(tbl_dm_id, lvl_text) {
  dplyr::tbl(con, "dimension_levels") %>%
    dplyr::filter(tab_dim_id == tbl_dm_id,
                  level_text == lvl_text) %>%
    dplyr::pull(level_value)
}

#' @rdname get_stuff
#' @return character code from level value code
#' @keywords internal
get_interval_id <- function(code_no) {
  tbl_id <- get_table_id(code_no)
  interval_lookupV <- setNames(c("Q", "M", "Y"), c("\\u010cETRTLETJE", "MESEC", "LETO"))
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::filter(time) %>%
    dplyr::collect() %>%
    dplyr::pull(dimension) -> interval_text
  interval_id <- ifelse(stringi::stri_escape_unicode(interval_text) %in% names(interval_lookupV),
                        getElement(interval_lookupV, stringi::stri_escape_unicode(interval_text)), NA)
  interval_id
}

#' @rdname get_stuff
#' @return numeric id of MERITVE dimension if it exists, else integer64(0)
#' @keywords internal
get_meritve_id <- function(tbl_id) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::filter(dimension == "MERITVE") %>%
    dplyr::pull(id)
}

#' @rdname get_stuff
#' @return numeric position of MERITVE dimension if it exists, else integer64(0)
#' @keywords internal
get_meritve_no <-function(tbl_id) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  time != TRUE) %>%
    dplyr::mutate(poz = dplyr::row_number()) %>%
    dplyr::filter(dimension == "MERITVE") %>%
    dplyr::pull(poz)
}

#' @rdname get_stuff
#' @return tibble with 4 cols including `level_value` and `unit_id`
#' @keywords internal
get_unit_levels_from_meritve <- function(meritve_dim_id){
  dplyr::tbl(con, "dimension_levels") %>%
    dplyr::filter(tab_dim_id == meritve_dim_id) %>%
    dplyr::collect() %>%
    dplyr::mutate(unit = regmatches(level_text, regexpr("(?<=[(]{1})([^)]+)(?=[)]{1}$)",
                                                        level_text, perl = TRUE))) %>%
    dplyr::select(-level_text) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(unit_id = get_unit_id(unit))
}

#' @rdname get_stuff
#' @return numeric id of valuenotes dimension if it exists, else integer64(0)
#' @keywords internal
get_valuenotes_id <- function(tbl_id, dim_name) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::filter(dimension == dim_name) %>%
    dplyr::pull(id)
}

#' @rdname get_stuff
#' @return numeric position of valuenotes dimension if it exists, else integer64(0)
#' @keywords internal
get_valuenotes_no <-function(tbl_id, dim_name) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  time != TRUE) %>%
    dplyr::mutate(poz = dplyr::row_number()) %>%
    dplyr::filter(dimension == dim_name) %>%
    dplyr::pull(poz)
}