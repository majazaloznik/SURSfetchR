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
#' @param file path to sql file
#' @inheritParams common_parameters
#'
#' @export
execute_sql_file <- function(con, file,
                                       schema = "test_platform"){
  sql <- readLines(file)
  sql <- gsub("test_platform\\.", sprintf("%s.", schema), sql)
  DBI::dbBegin(con)
  on.exit(  DBI::dbCommit(con))
  # split up SQL into a new statement for every ";"
  lapply(split(sql, cumsum(c(1,grepl(";",sql)[-length(sql)]))),
         function(x){
           DBI::dbExecute(con, paste(x, collapse = "\n"))
         })
}

#' Generic run sql file function
#'
#' Executes an sql file. Uses dbExecute, which requires single statements to
#' be passed i.e. cannot handle a whole file, so the function splits the file
#' on `;`. Potentially not safe.
#' Inspiration from `timeseriesdb` package by Matt Bannert.
#'
#' @param file path to sql file
#' @inheritParams common_parameters
#'
#' @export
execute_sql_functions_file <- function(con, file,
                                       schema = "test_platform"){
  sql <- readLines(file)
  sql <- gsub("test_platform\\.", sprintf("%s.", schema), sql)

  DBI::dbBegin(con)
  on.exit(DBI::dbCommit(con))
  # split up SQL into a new statement for every "plpgsql", which is at the end
  # of each function.
  lapply(split(sql, cumsum(c(1,grepl("plpgsql;",sql)[-length(sql)]))),
         function(x){
           DBI::dbExecute(con, paste(x, collapse = "\n"))
         })
}


#' Build all database tables
#'
#' Creates all the tables required to run the database in a given schema by
#' running the appropriate sql file. (Excluded from testing). Location of
#' sql file is in compiled package, hence no "inst/"
#'
#' @inheritParams common_parameters
#' @export
build_db_tables <- function(con, schema = "test_platform"){
  execute_sql_file(con,
                   file =system.file("sql/build_db.sql",
                               package = "SURSfetchR"),
                   schema = schema)
}




#' Construct and execute an SQL function call
#'
#' Constructs the call for the funciton `schema`.`fun_name` from the database
#' with the arguments `args` and returns the result.
#'
#' Inspiration from `timeseriesdb` package by Matt Bannert.
#'
#' @param fun_name character name of SQL function to call
#' @param args list of arguments, can be named (but then all of them have to be)
#'
#' @inheritParams common_parameters
#' @export
#'
#' @return value of `dbGetQuery(con, "SELECT * FROM schema.fun_name($args)")$fun_name`

sql_function_call <- function(con,
                              fun_name,
                              args,
                              schema = "test_platform") {
  args_pattern <- ""
  if(!is.null(args)) {
    args[sapply(args, is.null)] <- NA
    args_pattern <- sprintf("$%d", seq(length(args)))

    if(!is.null(names(args))) {
      args_pattern <- paste(
        sprintf("%s :=", names(args)),
        args_pattern
      )
    }
    args_pattern <- paste(args_pattern, collapse = ", ")
  }

  query <- sprintf("SELECT * FROM %s.%s(%s)",
                   DBI::dbQuoteIdentifier(con, schema),
                   DBI::dbQuoteIdentifier(con, fun_name),
                   args_pattern)

  res <- DBI::dbGetQuery(con, query, unname(args))

  res
}
