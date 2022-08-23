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
#'
#' @return side effect is writing to the database.
#' @export

write_row_table <- function(code_no, dbtable, con, sql_statement, counter) {
  checkmate::qassert(code_no, "S[5,11]")
  code_no <- sub(".PX$", "", code_no)
  code_no <- sub(".px$", "", code_no)
  tryCatch({if(!dbtable %>% select(code) %>%  filter(code == code_no)
               %>%  collect() %>% nrow()){
    mtdt <- get_px_metadata(code_no)
    dbExecute(con, sql_statement, list(mtdt$code,
                                       mtdt$name,
                                       mtdt$source,
                                       mtdt$url,
                                       "",
                                       mtdt$notes))
    counter <- counter + 1
    return(counter)
  } else {return(counter)}},
  error = function(cnd) {
    message(paste("INSERT into table table didn't work for",
                  "code ", code_no, ". Here is the original error: \n \n",
                  cnd))
    return(counter)}
  )
}



#' Write categories for a single table to the category table
#'
#' Helper function that extracts the field hierarchy from the full
#' hierarchy data.frame, and fills up the category table with field ids and
#' their parents. Gets run from \link[SURSfetchR]{write_multiple_rows}. Checks
#' id-parent_id combo is not already in the
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
#' @return
#' @export
#'
#' @examples
#'
write_row_category <- function(code_no, dbcategory, con, sql_statement, counter, full) {
  checkmate::qassert(code_no, "S[5,11]")
  code_no <- sub(".PX$", "", code_no)
  code_no <- sub(".px$", "", code_no)
  id_no <- unique(full$id[full$name == code_no])
  rows <- get_row(id_no, full) %>%
    mutate(parent_id = as.numeric(parent_id)) %>%
    arrange(parent_id)
  counter_i = 0
  for (i in seq_len(nrow(rows))){
    tryCatch({


      dbExecute(con, sql_statement, list(rows[i,]$id,
                                         rows[i,]$name,
                                         rows[i,]$parent_id,
                                         rows[i,]$source_id))
      counter_i <- counter_i + 1
      counter <- counter + 1
    },
    error = function(cnd) {
    }
    )
  }
  message(paste(counter_i, "new rows categories inserted for matrix ", code_no))
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
  message(paste(counter, "new rows inserted into table ", table_name))
}
