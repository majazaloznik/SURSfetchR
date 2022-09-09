#' Prepare table to insert into `table` table
#'
#' This one is really straightforward, and slightly superfluos, since it just
#' uses the \link[SURSfetchR]{get_px_metadata} function and removes two columns.
#' Returns table ready to insert into the `table` table
#'
#' @param code_no the matrix code (e.g. 2300123S)
#'
#' @return a dataframe with the `code`, `name`, `source`, `url`, and `notes` columns
#' for this table.
#' @export
prepare_table_table <- function(code_no) {
  get_px_metadata(code_no) %>%
    select(-created, -valuenotes)
}


#' Write categories for a single table to the `category` table
#'
#' Helper function that extracts all the parent categories from the full
#' hierarchy data.frame, and fills up the category table with field ids and
#' their names. Gets run from \link[SURSfetchR]{write_multiple_rows}. Uses original
#' id's from SURS, keeping them unique by adding the source_id to the constraint.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param full full field hierarchy with parent_ids et al, output from
#' \link[SURSfetchR]{get_full_structure}
#'
#' @return incremented counter, side effect is writing to the database.
#' @export
write_row_category <- function(code_no, con, sql_statement, counter, full) {
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
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param full full field hierarchy with parent_ids et al, output from
#' \link[SURSfetchR]{get_full_structure}
#'
#' @return incremented counter, side effect is writing to the database.
#' @export

write_row_category_relationship <- function(code_no, con, sql_statement, counter, full) {
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
write_row_category_table <- function(code_no, con, sql_statement, counter, full) {
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
write_row_table_dimensions <- function(code_no, con, sql_statement, counter, ...) {
  get_table_id(code_no, con) -> tbl_id
  tmp <- get_table_levels(code_no, con) %>%
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
write_row_dimension_levels <- function(code_no, con, sql_statement, counter, ...) {
  get_table_id(code_no, con) -> tbl_id
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::filter(time == FALSE) %>%
    dplyr::select(dimension, id) %>%
    collect() -> dim_ids

  tmp <- get_table_levels(code_no, con) %>%
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
write_row_unit <- function(code_no, con, sql_statement, counter, ...) {
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
#'
#' @return a dataframe with three columns: series_title, series_code and unit_id as
#' well as the same number of rows as there are series
#' @export

prepare_series_table <- function(code_no){
  get_table_id(code_no, con) -> tbl_id
  get_interval_id(code_no, con) -> interval_id
  get_single_unit_from_px(code_no, con) -> unit_id
  expand_to_level_codes(code_no, unit_id, con) -> expanded_level_codes
  if(is.na(unit_id)) {
    get_meritve_id(tbl_id, con) -> meritve_dim_id
    get_meritve_no(tbl_id, con) -> meritve_dim_no
    if(length(meritve_dim_id) == 1) {# if MERITVE EXIST
      get_unit_levels_from_meritve(meritve_dim_id, con) -> units_by_meritve_levels
      add_meritve_level_units(expanded_level_codes, meritve_dim_no,
                              units_by_meritve_levels) -> expanded_level_codes
    } else { # if valuenotes exist
      get_valuenotes_from_px(code_no, tbl_id, con) -> units_by_levels
      get_valuenotes_id(tbl_id, units_by_levels$dim_name[1]) -> valuenotes_dim_id
      get_valuenotes_no(tbl_id,  units_by_levels$dim_name[1]) -> valuenotes_dim_no
      if(length(valuenotes_dim_id) == 1){
        add_valuenotes_level_units(expanded_level_codes, valuenotes_dim_no,
                                   units_by_levels) -> expanded_level_codes}
    }
  }

  expanded_level_codes %>%
    tidyr::unite("series_code", dplyr::starts_with("Var"), sep = "--") %>%
    dplyr::mutate(series_code = paste0("SURS--", code_no, "--", series_code, "--",interval_id)) %>%
    cbind(expand_to_series_titles(code_no, con)) %>%
    dplyr::mutate(table_id = tbl_id,
                  interval_id = interval_id)
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
write_row_series_levels <- function(code_no, con, sql_statement, counter, ...) {
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

