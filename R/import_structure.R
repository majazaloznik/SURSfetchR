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
get_table_id <- function(code_no, con) {
  dplyr::tbl(con, "table") %>%
    dplyr::filter(code == code_no) %>%
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
#' @return character code from level value code
#' @keywords internal
get_interval_id <- function(code_no, con) {
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
get_meritve_id <- function(tbl_id, con) {
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::filter(dimension == "MERITVE") %>%
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
    dplyr::pull(poz)
}

#' @rdname get_stuff
#' @return tibble with 4 cols including `level_value` and `unit_id`
#' @keywords internal
get_unit_levels_from_meritve <- function(meritve_dim_id, con){
  dplyr::tbl(con, "dimension_levels") %>%
    dplyr::filter(tab_dim_id == meritve_dim_id) %>%
    dplyr::collect() %>%
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
    dplyr::pull(poz)
}


#' Family of helper functions to extract units from VALUENOTES list
#'
#' Some SURS matrices have the units for individual levels saved in a list
#' in the VALUENOTES slot of the px. metadata. These need to be regexed outta there
#' with the following three functions.
#'
#' @param x a character string, either the element name (for dimension and level)
#' or the element itself (for the unit)
#' @return character string of cleaned up dimension name, dimension level label or
#' unit name (which is what we are after in the end).
#' @rdname valuenotes
#' @keywords internal
get_valuenotes_dimension <- function(x){
  x <- regmatches(x, regexpr("[A-Z.]+(?=\\.)", x, perl = TRUE))
  gsub( "\\.", " ", x)
}

#' @rdname valuenotes
#' @keywords internal
get_valuenotes_level <- function(x){
  y <- gsub( "\\.", " ", x)
  gsub(paste0(get_valuenotes_dimension(x), " "), "", y)
}

#' @rdname valuenotes
#' @keywords internal
get_valuenotes_unit <- function(x, con){
  y <- regmatches(x, regexpr("(?<=Enota: ).+", x, perl = TRUE))
  y <- gsub( "\\.", "", y)
  unit_name <- gsub("\" \"", "", y)
  as.numeric(get_unit_id(unit_name, con))
}

#' Expanding from levels to series codes and titles
#'
#' These two helper functions take a set of non-time levels for a single table
#' and expand the grid to get all of their combinations and then either return
#' a dataframe with columns for each level code, or one where the level texts
#' have been concatenated into the series titles.
#' @param code_nopx. code e.g. 0300230S
#' @param unit_id num text of unit id
#' @return dataframe with expanded levels, one column per non-time dimension plus
#' unit_id for the level codes and sinle column with series titles for the other one.
#' @rdname expanding
#' @keywords internal
expand_to_level_codes <- function (code_no, unit_id) {
  get_table_levels(code_no) %>%
    dplyr::filter(!time) %>%
    dplyr::pull(levels) %>%
    purrr::map("values") %>%
    expand.grid(stringsAsFactors = FALSE) %>%
    dplyr::mutate(unit_id = unit_id)
}
#' @rdname expanding
#' @keywords internal
expand_to_series_titles <- function(code_no){
  get_table_levels(code_no) %>%
    dplyr::filter(!time) %>%
    dplyr::pull(levels) %>%
    purrr::map("valueTexts") %>%
    expand.grid() %>%
    tidyr::unite("series_title", dplyr::everything(), sep = " - ")
}

#' Joining the unit tables from either meritve or valuenotes.
#'
#' These two helper functions take the expanded level code tables and
#' join the appropriate unit_ids from either the meritve or the valuenotes
#' tables.
#'
#' @param expanded_level_codes dataframe output of \link[SURSfetchR]{expand_to_level_codes}
#' @param meritve_dim_no numeric output of \link[SURSfetchR]{get_meritve_no}
#' @param units_by_meritve_levels dataframe output of \link[SURSfetchR]{get_unit_levels_from_meritve}
#' @param valuenotes_dim_no numeric output of \link[SURSfetchR]{get_valuenotes_no}
#' @param units_by_levels dataframe output of \link[SURSfetchR]{get_unit_levels_from_meritve}
#' @return datafram from expanded_level_codes with added `unit_id` column
#'
#' @rdname add_units
#' @keywords internal
add_meritve_level_units <- function(expanded_level_codes, meritve_dim_no,
                                    units_by_meritve_levels){
  expanded_level_codes %>%
    dplyr::select(-unit_id) %>%
    dplyr::rename("level_value" := !!(paste0("Var", meritve_dim_no))) %>%
    dplyr::left_join(units_by_meritve_levels, by = c("level_value" = "level_value")) %>%
    dplyr::rename(!!(paste0("Var", meritve_dim_no)) := "level_value") %>%
    dplyr::select(-tab_dim_id, -unit)
}

#' @rdname add_units
#' @keywords internal
add_valuenotes_level_units <- function(expanded_level_codes, valuenotes_dim_no,
                                       units_by_levels){
  expanded_level_codes %>%
    dplyr::select(-unit_id) %>%
    dplyr::rename("level_value" := !!(paste0("Var", valuenotes_dim_no))) %>%
    dplyr::left_join(units_by_levels, by = c("level_value" = "level_value")) %>%
    dplyr::rename(!!(paste0("Var", valuenotes_dim_no)) := "level_value") %>%
    dplyr::select(-dim_name, -level_text, -tab_dim_id)
}


#' Write a single row to the `table` table for SURS
#'
#' Helper function that extracts metadata from the API and inserts it into the
#' sql statement and then gets written to the `table` table in the connection.
#'
#' Checks to make sure the code_no doesn't exist in the table already.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param dbtable the `tbl()` link to the table we are inserting to
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param ... just here, because other funs in this family have extra parameters
#' passed to them and i cannot use map unless this one also has this option.
#'
#' @return side effect is writing to the database.
#' @export

write_row_table <- function(code_no, dbtable, con, sql_statement, counter, ...) {
  checkmate::qassert(code_no, "S[5,11]")
  code_no <- sub(".PX$", "", code_no)
  code_no <- sub(".px$", "", code_no)
  tryCatch({if(!dbtable %>% select(code) %>%  filter(code == code_no)
               %>%  collect() %>% nrow()){
    mtdt <- get_px_metadata(code_no)
    DBI::dbExecute(con, sql_statement, list(mtdt$code,
                                       mtdt$name,
                                       mtdt$source,
                                       mtdt$url,
                                       "",
                                       mtdt$notes))
    counter <- counter + 1
    message(paste("new row inserted for matrix ", code_no))
    return(counter)
  } else {return(counter)}},
  error = function(cnd) {
    message(paste("INSERT into table table didn't work for",
                  "code ", code_no, ". Here is the original error: \n \n",
                  cnd))
    return(counter)}
  )
}


#' Write categories for a single table to the `category` table
#'
#' Helper function that extracts all the parent categories from the full
#' hierarchy data.frame, and fills up the category table with field ids and
#' their names. Gets run from \link[SURSfetchR]{write_multiple_rows}. Uses original
#' id's from SURS, keeping them unique by adding the source_id to the constraint.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param dbcategory tbl() link to db table, although this one isn't actually used
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param full full field hierarchy with parent_ids et al, output from
#' \link[SURSfetchR]{get_full_structure}
#'
#' @return incremented counter, side effect is writing to the database.
#' @export
write_row_category <- function(code_no, dbcategory, con, sql_statement, counter, full) {
  checkmate::qassert(code_no, "S[5,11]")
  code_no <- sub(".PX$", "", code_no)
  code_no <- sub(".px$", "", code_no)
  id_no <- unique(full$id[full$name == code_no])
  rows <- get_row(id_no, full) %>%
    select(-parent_id)
  counter_i = 0
  for (i in seq_len(nrow(rows))){
    tryCatch({

      DBI::dbExecute(con, sql_statement, list(rows[i,]$id,
                                         rows[i,]$name,
                                         rows[i,]$source_id))
      counter_i <- counter_i + 1
      counter <- counter + 1
    },
    error = function(cnd) {
    }
    )
  }
  message(paste(counter_i, "new categories inserted for matrix ", code_no))
  return(counter)
}

#' Write categories for a single table to the `category_relationship` table
#'
#' Helper function that extracts the field hierarchy from the full
#' hierarchy data.frame, and fills up the category table with field ids and
#' their parents. Gets run from \link[SURSfetchR]{write_multiple_rows}.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param dbcategory tbl() link to db table, although this one isn't actually used
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param full full field hierarchy with parent_ids et al, output from
#' \link[SURSfetchR]{get_full_structure}
#'
#' @return incremented counter, side effect is writing to the database.
#' @export

write_row_category_relationship <- function(code_no, dbcategory, con, sql_statement, counter, full) {
  checkmate::qassert(code_no, "S[5,11]")
  code_no <- sub(".PX$", "", code_no)
  code_no <- sub(".px$", "", code_no)
  id_no <- unique(full$id[full$name == code_no])
  rows <- get_row(id_no, full) %>%
    dplyr::mutate(parent_id = as.numeric(parent_id)) %>%
    dplyr::arrange(parent_id)
  counter_i = 0
  for (i in seq_len(nrow(rows))){
    tryCatch({

      DBI::dbExecute(con, sql_statement, list(rows[i,]$id,
                                         rows[i,]$parent_id,
                                         rows[i,]$source_id))
      counter_i <- counter_i + 1
      counter <- counter + 1
    },
    error = function(cnd) {
    }
    )
  }
  message(paste(counter_i, "new category relationships inserted for matrix ", code_no))
  return(counter)
}

#' Write categories for a single table to the `category_table` table
#'
#' Helper function that extracts the parent category for each table from the full
#' hierarchy data.frame, and fills up the category_table table with the table ids and
#' their categories (parents). Gets run from \link[SURSfetchR]{write_multiple_rows}.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param dbcategory_table tbl() link to db table, although this one isn't actually used
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param full full field hierarchy with parent_ids et al, output from
#' \link[SURSfetchR]{get_full_structure}
#'
#' @return incremented counter, side effect is writing to the database.
#' @export
#'
write_row_category_table <- function(code_no, dbcategory_table, con, sql_statement, counter, full) {
  checkmate::qassert(code_no, "S[5,11]")
  code_no <- sub(".PX$", "", code_no)
  code_no <- sub(".px$", "", code_no)
  dplyr::tbl(con, "table") %>%
    dplyr::filter(code == code_no) %>%
    dplyr::pull(id) -> table_id
  rows <- full %>%
    dplyr::filter(name == code_no) %>%
    dplyr::select(category_id = parent_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(table_id = table_id,
           source_id = 1)
  counter_i = 0
  for (i in seq_len(nrow(rows))){
    tryCatch({

      DBI::dbExecute(con, sql_statement, list(rows[i,]$table_id,
                                         rows[i,]$category_id,
                                         1))
      counter_i <- counter_i + 1
      counter <- counter + 1
    },
    error = function(cnd) {
      print(cnd)
    }
    )
  }
  message(paste(counter_i, "new category-table rows inserted for matrix ", code_no))
  return(counter)
}


#' Write categories for a single table to the `table_dimensions` table
#'
#' Helper function that extracts the dimensions for each table and their "time" status.
#' Gets run from \link[SURSfetchR]{write_multiple_rows}.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param dbtable_dimensions tbl() link to db table, although this one isn't actually used
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param ...  just here, because other funs in this family have extra parameters
#' passed to them and i cannot use map unless this one also has this option.
#'
#' @return incremented counter, side effect is writing to the database.
#'
#' @export
write_row_table_dimensions <- function(code_no, dbtable_dimensions, con, sql_statement, counter, ...) {
  get_table_id(code_no, con) -> tbl_id
  tmp <- get_table_levels(code_no) %>%
    dplyr::mutate(table_id = tbl_id) %>%
    dplyr::select(table_id, dimension_name, time)
  counter_i = 0
  for (i in seq_len(nrow(tmp))){
    tryCatch({
      DBI::dbExecute(con, sql_statement, list(tmp[i,]$table_id,
                                         tmp[i,]$dimension_name,
                                         tmp[i,]$time))
      counter_i <- counter_i + 1
      counter <- counter + 1
    },
    error = function(cnd) {
      print(cnd)
    }
    )
  }
  message(paste(counter_i, "new category-table rows inserted for matrix ", code_no))
  return(counter)
}

#' Write dimension levels for a single table to the `dimension_levels` table
#'
#' Helper function that extracts the levels for all the dimensions for each
#' table and get their code and text.
#' Gets run from \link[SURSfetchR]{write_multiple_rows}.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param dbtable_dimensions tbl() link to db table, although this one isn't actually used
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param ...  just here, because other funs in this family have extra parameters
#' passed to them and i cannot use map unless this one also has this option.
#'
#' @return incremented counter, side effect is writing to the database.
#'
#' @export
write_row_dimension_levels <- function(code_no, dbtable_dimensions, con, sql_statement, counter, ...) {
  get_table_id(code_no, con) -> tbl_id
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::filter(time == FALSE) %>%
    dplyr::select(dimension, id) %>%
    collect() -> dim_ids

  tmp <- get_table_levels(code_no) %>%
    tidyr::unnest(levels) %>%
    dplyr::mutate(table_id = tbl_id) %>%
    dplyr::select(dimension_name, values, valueTexts) %>%
    dplyr::inner_join(dim_ids, by = c("dimension_name" = "dimension"))

  counter_i = 0
  for (i in seq_len(nrow(tmp))){
    tryCatch({
      DBI::dbExecute(con, sql_statement, list(tmp[i,]$id,
                                         tmp[i,]$values,
                                         tmp[i,]$valueTexts))
      counter_i <- counter_i + 1
      counter <- counter + 1
    },
    error = function(cnd) {
      print(cnd)
    }
    )
  }
  message(paste(counter_i, "new dimension-level rows inserted for matrix ", code_no))
  return(counter)
}

#' Write units into to the `units` table
#'
#' Helper function that extracts the units for each table from the px metadata.
#' Gets run from \link[SURSfetchR]{write_multiple_rows}.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param dbunits tbl() link to db table, although this one isn't actually used
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param ...  just here, because other funs in this family have extra parameters
#' passed to them and i cannot use map unless this one also has this option.
#'
#' @return incremented counter, side effect is writing to the database.
#'
#' @export
write_row_unit <- function(code_no, dbunits, con, sql_statement, counter, ...) {
  tmp <- data.frame(strsplit(get_px_metadata(code_no)$units, ", ")) %>%
    mutate_all(tolower)
  counter_i = 0
  for (i in seq_len(nrow(tmp))){
    tryCatch({
      dbExecute(con, sql_statement, list(tmp[i,1]))
      counter_i <- counter_i + 1
      counter <- counter + 1
    },
    error = function(cnd) {
      print(cnd)
    }
    )
  }
  message(paste(counter_i, "new units inserted for matrix ", code_no))
  return(counter)
}



#' Prepare table to insert into `series` table
#'
#' This is a big one. Prepares the table to insert into the series table, which
#' along expanding the leves to get all the series and their codes as well, which
#' include also figuring out their time interval, this function also tries to
#' extract the unit for each series, which can get pretty tricky. There are generally
#' three options:
#' 1. either the whole table and therefore all the series within it have a single unit
#' which we get from the .px metadata.
#' 2. or there is a MERITVE dimensions, which might (?) contain the units for each
#' level in parentheses at the end of the label. So we get if from there
#' 3. ooor, there is no MERITVE dimension, and then there is (?) a slot in the .px
#' data called `valuenotes`, which contains the units and again there is an even
#' more convoluted way to get them out and know which level values they apply to.
#'
#' @param code_no character obect of the matrix code (e.g. 2300123S)
#' @param con postgresql connection to the database.
#'
#' @return a dataframe with three columns: series_title, series_code and unit_id as
#' well as the same number of rows as there are series
#' @export

prepare_series_table <- function(code_no, con){
  get_table_id(code_no, con) -> tbl_id
  get_interval_id(code_no, con) -> interval_id
  get_single_unit_from_px(code_no, con) -> unit_id
  expand_to_level_codes(code_no, unit_id) -> expanded_level_codes
  if(is.na(unit_id)) {
    get_meritve_id(tbl_id, con) -> meritve_dim_id
    get_meritve_no(tbl_id, con) -> meritve_dim_no
    if(length(meritve_dim_id) == 1) {# if MERITVE EXIST
      get_unit_levels_from_meritve(meritve_dim_id, con) -> units_by_meritve_levels
      add_meritve_level_units(expanded_level_codes, meritve_dim_no,
                              units_by_meritve_levels) -> expanded_level_codes
    } else { # if valuenotes exist
      get_valuenotes_from_px(code_no, tbl_id, con) -> units_by_levels
      get_valuenotes_id(tbl_id, units_by_levels$dim_name[1], con) -> valuenotes_dim_id
      get_valuenotes_no(tbl_id,  units_by_levels$dim_name[1], con) -> valuenotes_dim_no
      if(length(valuenotes_dim_id) == 1){
        add_valuenotes_level_units(expanded_level_codes, valuenotes_dim_no,
                                   units_by_levels) -> expanded_level_codes}
    }
  }

  expanded_level_codes %>%
    tidyr::unite("series_code", dplyr::starts_with("Var"), sep = "--") %>%
    dplyr::mutate(series_code = paste0("SURS--", code_no, "--", series_code, "--",interval_id)) %>%
    cbind(expand_to_series_titles(code_no))
}

#' Write series into to the `series` table
#'
#' Helper function that extracts the individual series = combinations of dimension
#' levels from the API levels tables. .
#' Gets run from \link[SURSfetchR]{write_multiple_rows}.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param dbseries tbl() link to db table, although this one isn't actually used
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param ...  just here, because other funs in this family have extra parameters
#' passed to them and i cannot use map unless this one also has this option.
#'
#' @return incremented counter, side effect is writing to the database.
#'
#' @export
write_row_series <- function(code_no, dbseries, con, sql_statement, counter, ...) {
  get_table_id(code_no, con) -> tbl_id
  get_interval_id(code_no, con) -> interval_id
  prepare_series_table(code_no, con) -> tmp

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



#' Write series-level combinations into to the `series_levels` table
#'
#' Helper function that extracts the individual levels for each series and
#' gets the correct dimension id for each one and the correct series id to
#' keep with the constraints.
#'
#' Gets run from \link[SURSfetchR]{write_multiple_rows}.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param dbseries_levels tbl() link to db table, although this one isn't actually used
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param ...  just here, because other funs in this family have extra parameters
#' passed to them and i cannot use map unless this one also has this option.
#'
#' @return incremented counter, side effect is writing to the database.
#'
#' @export
write_row_series_levels <- function(code_no, dbseries_levels, con, sql_statement, counter, ...) {
  get_table_id(code_no, con) -> tbl_id

  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  time != TRUE) %>%
    dplyr::pull(id) -> dimz

  dplyr::tbl(con, "series") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::collect() %>%
    dplyr::select(table_id, id, code) %>%
    tidyr::separate(code, into = c("x1", "x2", paste0(dimz), "x3"), sep = "--") %>%
    dplyr::select(series_id = id,  paste0(dimz)) %>%
    tidyr::pivot_longer(-series_id, names_to = "tab_dim_id" ) -> tmp

  counter_i = 0
  for (i in seq_len(nrow(tmp))){
    tryCatch({
      dbExecute(con, sql_statement, list(tmp[i,]$series_id,
                                         tmp[i,]$tab_dim_id,
                                         tmp[i,]$value))
      counter_i <- counter_i + 1
      counter <- counter + 1
    },
    error = function(cnd) {
      print(cnd)
    }
    )
  }
  message(paste(counter_i, "new series-level combos inserted for matrix ", code_no))
  return(counter)
}



#' Umbrella function to write multiple rows into the a postgres table
#'
#' Takes a column of SURS codes from the master_list_code and for each row
#' gets the metadata and writes it to the appropriate table  using the
#' passed sql statement and the appropriate function from the `write_row`
#' family of functions for each single row.
#'
#' @param master_list_surs dataframe with at minimum a "code" column with
#' the SURS matrix code for each table to be inserted.
#'
#' @param con a connection to a postgres database with an approproate `table`
#' table.
#' @param table_name character name of table in db, also used to get the
#' relevant function to write a single row.
#' @param sql_statement character string to write single row to the database.
#' @param ... other parameters that get passed on to the `write_row_` type
#' function.
#'
#' @return nothing - side effect is writing to the database, and outputing message
#' to say how many rows were inserted.
#' @export
#'
write_multiple_rows <- function(master_list_surs, con, table_name, sql_statement, ...) {
  dbtable <- tbl(con, table_name)

  counter <- 0
  for (i in seq(nrow(master_list_surs))){
    counter <- get(paste0("write_row_", table_name))(master_list_surs$code[i], dbtable, con, sql_statement, counter, ...)
  }
  message(paste(counter, "new rows inserted into table ", table_name, "\n"))
}
