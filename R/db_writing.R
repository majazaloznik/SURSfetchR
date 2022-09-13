#' Write a single row to the `table` table for SURS
#'
#' Helper function that extracts metadata from the API and inserts it into the
#' sql statement and then gets written to the `table` table in the connection.
#'
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @param sql_statement the sql statement to insert the values
#' @param counter integer counter used in  \link[SURSfetchR]{write_multiple_rows}
#' to count how many successful rows were inserted.
#' @param ... just here, because other funs in this family have extra parameters
#' passed to them and i cannot use map unless this one also has this option.
#'
#' @return side effect is writing to the database.
#' @export

write_row_table <- function(code_no, con, sql_statement, counter, ...) {
  tmp <- prepare_table_table(code_no)
  tryCatch({
    DBI::dbExecute(con, sql_statement, list(tmp$code,
                                            tmp$name,
                                            tmp$source,
                                            tmp$url,
                                            tmp$notes))
    counter <- counter + 1
    message(paste("new row inserted into `table` for matrix ", code_no))
  },
  error = function(cnd) {
    print(cnd)
  }
  )
  return(counter)
}

#' Write a single row to the `category` table for SURS
#'
#' Helper function that extracts all the parent categories from the full
#' hierarchy data.frame, and fills up the category table with field ids and
#' their names. Gets run from \link[SURSfetchR]{write_multiple_rows}. Uses original
#' id's from SURS, keeping them unique by adding the source_id to the constraint.
#'#'
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
  tmp <- prepare_category_table(code_no, full)
  counter_i = 0
  for (i in seq_len(nrow(tmp))){
    tryCatch({

      DBI::dbExecute(con, sql_statement, list(tmp[i,]$id,
                                              tmp[i,]$name,
                                              tmp[i,]$source_id))
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
  tmp <- prepare_category_relationship_table(code_no, full)
  counter_i = 0
  for (i in seq_len(nrow(tmp))){
    tryCatch({
      DBI::dbExecute(con, sql_statement, list(tmp[i,]$id,
                                              tmp[i,]$parent_id,
                                              tmp[i,]$source_id))
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
  tmp <- prepare_category_table_table(code_no, full, con)
   counter_i = 0
  for (i in seq_len(nrow(tmp))){
    tryCatch({

      DBI::dbExecute(con, sql_statement, list(tmp[i,]$table_id,
                                              tmp[i,]$category_id,
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


#' Write series into to the `series` table
#'
#' Helper function that extracts the individual series = combinations of dimension
#' levels from the API levels tables. .
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
write_row_series <- function(code_no, con, sql_statement, counter, ...) {

  prepare_series_table(code_no) -> tmp

  counter_i = 0
  for (i in seq_len(nrow(tmp))){
    tryCatch({
      dbExecute(con, sql_statement, list(tmp[i,]$table_id,
                                         tmp[i,]$series_title,
                                         tmp[i,]$series_code,
                                         tmp[i,]$interval_id,
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

  counter <- 0
  for (i in seq(nrow(master_list_surs))){
    counter <- get(paste0("write_row_", table_name))(master_list_surs$code[i], con, sql_statement, counter, ...)
  }
  message(paste(counter, "new rows inserted into table ", table_name, "\n"))
}
