#' Insert table structure data for a new table
#'
#' When a new table (in SURS speak 'matrix") is added, a set of nine
#' tables need to be populated with apropriate metadata about that table.
#' This umbrella function calls the respective SQL functions for each
#' of the nine tables.
#'
#' @inheritParams common_parameters
#' @param full full field hierarchy with parent_ids et al, output from
#'  \link[SURSfetchR]{get_full_structure}
#'
#' @return list of tables with counts for each inserted row.
#' @export
#'
#' @examples
#' \dontrun{
#' purrr::walk(master_list_surs$code, ~insert_new_table_structures(.x, con, full))
#' }

insert_new_table_structures <- function(code_no, con, full) {
  res <- list()
  res[[1]] <- sql_function_call(con,
                                "insert_new_table",
                                as.list(prepare_table_table(code_no)))
  res[[2]] <- sql_function_call(con,
                                "insert_new_category",
                                as.list(prepare_category_table(code_no, full)))
  res[[3]] <- sql_function_call(con,
                                "insert_new_category_relationship",
                                as.list(prepare_category_relationship_table(code_no, full)))

  res[[4]] <- sql_function_call(con,
                                "insert_new_category_table",
                                as.list(prepare_category_table_table(code_no, full, con)))
  res[[5]] <- sql_function_call(con,
                                "insert_new_table_dimensions",
                                as.list(prepare_table_dimensions_table(code_no, con)))
  res[[6]] <- sql_function_call(con,
                                "insert_new_dimension_levels",
                                as.list(prepare_dimension_levels_table(code_no, con)))
  res[[7]] <- sql_function_call(con,
                                "insert_new_unit",
                                unname(as.list(prepare_unit_table(code_no, con))))

  res[[8]] <-  sql_function_call(con,
                                 "insert_new_series",
                                 unname(as.list(prepare_series_table(code_no, con))))
  res[[9]] <- sql_function_call(con,
                                "insert_new_series_levels",
                                unname(as.list(prepare_series_levels_table(code_no, con))))
  res
}


#' Insert new data for a table i.e. a vintage
#'
#' When new data for a table (in SURS speak 'matrix") is added, these are new
#' vintages. This function adds a set of new vintages and their corresponding
#' data points and flags to the database, by calling the respective SQL functions
#' for each of these tables.
#'
#' @inheritParams common_parameters
#'
#' @return list of tables with counts for each inserted row.
#' @export
#'
#' @examples
#' \dontrun{
#' purrr::walk(master_list_surs$code, ~insert_new_data(.x, con))
#' }
insert_new_data <- function(code_no, con) {
  res <- list()
  res[[1]] <- sql_function_call(con,
                                "insert_new_vintage",
                                unname(as.list(prepare_vintage_table(code_no ,con))))

  insert_data_points(code_no, con)
  res
}
