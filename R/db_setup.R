#' Build all database tables
#'
#' Creats all the tables required to run the database in a given schema.
#' Inspiration from `timeseriesdb` package by Matt Bannert.
#'
#' @param con PostgreSQL connection object created by the RPostgres package.
#' @param schema character schema name, default is test_platform
#'
#' @export
build_db_tables <- function(con, schema = "test_platform"){
  sql <- readLines(system.file("sql/build_db.sql",
                               package = "SURSfetchR"))
  sql <- gsub("test_platform\\.", sprintf("%s.", schema), sql)
  DBI::dbBegin(con)
  # split up SQL by a new set of lines for every ";"
  lapply(split(sql, cumsum(c(1,grepl(";",sql)[-length(sql)]))),
         function(x){
           DBI::dbExecute(con, paste(x, collapse = "\n"))
         })
  DBI::dbCommit(con)
}


