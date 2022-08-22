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
#' @param counter integer counter used in  \link[SURSfetchR]{write_rows_table}
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


#' Umbrella function to write multiple rows into the `table` table
#'
#' Takes a column of SURS codes from the master_list_code and for each row
#' gets the metadata and writes it to the `table` table  using the
#' \link[SURSfetchR]{write_row_table} function for each single row.
#'
#' @param master_list_surs dataframe with at minimum a "code" column with
#' the SURS matrix code for each table to be inserted.
#'
#' @param con a connection to a postgres database with an approproate `table`
#' table.
#'
#' @return nothing - side effect is writing to the database.
#' @export
write_rows_table <- function(master_list_surs, con) {
  dbtable <- tbl(con, 'table')
  sql_statement <- paste("INSERT INTO \"table\"",
                         "(code, name, source_id, url, description, notes)",
                         "VALUES",
                         "($1, $2, $3, $4, $5, $6)")
  counter <- 0
  for (i in seq(nrow(master_list_surs))){
    counter <- write_row_table(master_list_surs$code[i], dbtable, con, sql_statement, counter)
  }
  message(paste(counter, "new rows inserted into table `table`"))
}
