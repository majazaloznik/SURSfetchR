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
#' @return numeric position of MERITVE dimension if it exists,
#' @keywords internal
get_meritve_no <-function(tbl_id, con, schema = "platform") {
  DBI::dbExecute(con, paste("set search_path to", schema ))
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  is_time != TRUE) %>%
    dplyr::mutate(poz = dplyr::row_number()) %>%
    dplyr::filter(dimension == "MERITVE") %>%
    dplyr::pull(poz) |>
    as.numeric()
}


#' @rdname get_stuff
#' @return tibble with 4 cols including `level_value` and `unit_id`
#' @keywords internal
get_unit_levels_from_meritve <- function(meritve_level_text, con, schema = "platform"){
  DBI::dbExecute(con, paste("set search_path to", schema))
  meritve_level_text %>%
    dplyr::mutate(tmp = regexpr("(?<=[(]{1})([^)]+)(?=[)]{1}$)",
                                level_text, perl = TRUE)) %>%
    dplyr::mutate(unit = ifelse(tmp == -1, "", regmatches(level_text, tmp))) %>%
    dplyr::mutate(unit = gsub("^v ", "", unit)) %>%
    dplyr::select(-level_text) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(unit_id = UMARaccessR::sql_get_unit_id_from_unit_name(tolower(unit), con, schema))
}


#' Helper to get interval id from lookup vector
#'
#' This is not a db reading function, but it kinda fits with them, simply
#' because i'm using a lookup vector inside the function instead of creating
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

