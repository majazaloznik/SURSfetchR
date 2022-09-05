#' Helper fun to get correct table id from px code
#'
#' @param code_no px. code e.g. 0300230S
#'
#' @return numeric code from the table table
#' @keywords internal
get_table_id <- function(code_no) {
  dplyr::tbl(con, "table") %>%
    dplyr::filter(code == code_no) %>%
    dplyr::pull(id)
}

#' Helper fun to get correct code from unit text
#'
#' @param unit char text of unit label
#'
#' @return numeric code from db table unit
#' @keywords internal
get_unit_id <- function(unit){
  unit <- tolower(unit)
  dplyr::tbl(con, "unit") %>%
    dplyr::filter(name == unit) %>%
    pull(id)
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
  get_table_id(code_no) -> tbl_id
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
  get_table_id(code_no) -> tbl_id
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

  get_table_id(code_no) -> tbl_id

  lookupV <- setNames(c("Q", "M", "Y"), c("\\u010cETRTLETJE", "MESEC", "LETO"))
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::filter(time) %>%
    dplyr::collect() %>%
    dplyr::pull(dimension) -> int
  int_id <- ifelse(stringi::stri_escape_unicode(int) %in% names(lookupV),
                   getElement(lookupV, stringi::stri_escape_unicode(int)), NA)


  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    filter(dimension == "MERITVE") %>%
    pull(id) -> meritve_dim_id

  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::mutate(unit = dplyr::row_number()) %>%
    filter(dimension == "MERITVE") %>%
    pull(unit) -> meritve_dim_no

  if(length(meritve_dim_id) == 1) {
    dplyr::tbl(con, "dimension_levels") %>%
      dplyr::filter(tab_dim_id == meritve_dim_id) %>%
      dplyr::collect() %>%
      dplyr::mutate(unit = regmatches(level_text, regexpr("(?<=[(]{1})([^)]+)(?=[)]{1}$)",
                                                          level_text, perl = TRUE))) %>%
      dplyr::select(-level_text) %>%
      rowwise() %>%
      dplyr::mutate(unit = get_unit_id(unit)) -> tmp1
  }

  get_table_levels(code_no) %>%
    dplyr::filter(!time) %>%
    dplyr::pull(levels) %>%
    purrr::map("values") %>%
    expand.grid() %>%
    mutate(unit = unit) -> tmp2

  if(length(meritve_dim_id) == 1 & all(is.na(tmp2$level_value))) {
    tmp2 %>%
      select(-unit) %>%
      rename("unit" := !!(paste0("Var", meritve_dim_no))) %>%
      left_join(tmp1, by = c("unit" = "unit")) %>%
      select(-tab_dim_id) -> tmp2
  }

  tmp2 %>%
    tidyr::unite("code", dplyr::starts_with("Var"), sep = "--") %>%
    dplyr::mutate(code = paste0("SURS--", code_no, "--", code, "--",int_id)) %>%
    cbind(get_table_levels(code_no) %>%
            dplyr::filter(!time) %>%
            dplyr::pull(levels) %>%
            purrr::map("valueTexts") %>%
            expand.grid() %>%
            tidyr::unite("title", dplyr::everything(), sep = " - ")) -> tmp

  counter_i = 0
  for (i in seq_len(nrow(tmp))){
    tryCatch({
      dbExecute(con, sql_statement, list(tbl_id,
                                         tmp[i,]$title,
                                         tmp[i,]$code,
                                         int_id))
      counter_i <- counter_i + 1
      counter <- counter + 1
    },
    error = function(cnd) {
      print(cnd)
    }
    )
  }
  message(paste(counter_i, "new series inserted for matrix ", code_no))
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
  get_table_id(code_no) -> tbl_id

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
