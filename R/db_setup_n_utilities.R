#' Generic run sql file
#'
#' Executes an sql file. Uses dbExecute, which requires single statements to
#' be passed i.e. cannot handle a whole file, so the function splits the file
#' on `;`. Potentially not safe.
#' If file has stored procedures, use \link[SURSfetchR]{execute_sql_functions_file}
#' instead.
#'
#' Inspiration from `timeseriesdb` package by Matt Bannert.
#'
#' @param con PostgreSQL connection object created by the RPostgres package.
#' @param file path to sql file
#' @param schema character string of schema name, default is test_platform, subbed out
#' of the sql if need be
#'
#' @export
execute_sql_file <- function(con, file,
                                       schema = "test_platform"){
  sql <- readLines(file)
  sql <- gsub("test_platform\\.", sprintf("%s.", schema), sql)
  DBI::dbBegin(con)
  # split up SQL into a new statement for every ";"
  lapply(split(sql, cumsum(c(1,grepl(";",sql)[-length(sql)]))),
         function(x){
           DBI::dbExecute(con, paste(x, collapse = "\n"))
         })
  DBI::dbCommit(con)
}

#' Generic run sql file function
#'
#' Executes an sql file. Uses dbExecute, which requires single statements to
#' be passed i.e. cannot handle a whole file, so the function splits the file
#' on `;`. Potentially not safe.
#' Inspiration from `timeseriesdb` package by Matt Bannert.
#'
#' @param con PostgreSQL connection object created by the RPostgres package.
#' @param file path to sql file
#' @param schema character string of schema name, default is test_platform, subbed out
#' of the sql if need be
#'
#' @export
execute_sql_functions_file <- function(con, file,
                                       schema = "test_platform"){
  sql <- readLines(file)
  sql <- gsub("test_platform\\.", sprintf("%s.", schema), sql)
  DBI::dbBegin(con)
  # split up SQL into a new statement for every ";"
  lapply(split(sql, cumsum(c(1,grepl("plpgsql;",sql)[-length(sql)]))),
         function(x){
           DBI::dbExecute(con, paste(x, collapse = "\n"))
         })
  DBI::dbCommit(con)
}


#' Build all database tables
#'
#' Creates all the tables required to run the database in a given schema by
#' running the appropriate sql file. (Excluded from testing).
#'
#' @param con PostgreSQL connection object created by the RPostgres package.
#' @param schema character schema name, default is test_platform
#'
#' @export
build_db_tables <- function(con, schema = "test_platform"){
  execute_sql_file(con,
                   file =system.file("sql/build_db.sql",
                               package = "SURSfetchR"),
                   schema = schema)
}
