#' Import structural metadata for a SURS table
#'
#' Umbrella function that prepares and import all the metadata tables into
#' the database. This function is a bit slow because it downloads and imports
#' the full SURS category hierarchy, which takes a while, and i could
#' maybe have it already downloaded and stored, but it might change and i don't
#' know how often it does, and we don't insert new tables so often so for now
#' i'll leave it like this.
#'
#' @param code_no SURS code name of the table
#' @param con connection to database
#' @param schema schema name, defaults to platform
#' @param all_levels logical whether to let user use dimension selector, default
#' is true, which automatically selects all levels in all dimensions
#' @param keep_vintage logical indicating whether to keep vintages, defaults to F
#'
#' @returns nothing
#' @export
#'
SURS_import_structure <- function(code_no, con, schema = "platform", all_levels = TRUE,
                                  keep_vintage = FALSE) {
  message("Importing structure data into: ", code_no, " into schema ", schema)
  # Create list to store all results
  insert_results <- list()
  # prepare and insert table
  table_table <- prepare_table_table(code_no, keep_vintage)
  insert_results$table <- UMARimportR::insert_new_table_table(con, table_table, schema)
  message("Table insert: ", insert_results$table$count, " rows")
  # preapre and insert category table
  category_table <- prepare_category_table(code_no, full)
  insert_results$category <- UMARimportR::insert_new_category(con, category_table, schema)
  message("Category insert: ", insert_results$category$count, " rows")
  # prepare and insert category relationship table
  category_relationship_table <- prepare_category_relationship_table(code_no, full)
  insert_results$category_relantionship <- UMARimportR::insert_new_category_relationship(
    con, category_relationship_table, schema)
  message("Category relationship insert: ", insert_results$category_relantionship$count, " rows")
  # prepare and insert category table table
  category_table_table <- prepare_category_table_table(code_no, full, con, schema)
  insert_results$category_table <- UMARimportR::insert_new_category_table(
    con, category_table_table, schema)
  message("Category table insert: ", insert_results$category_table$count, " rows")
  # prepare and insert table dimension table
  table_dimension_table <- prepare_table_dimensions_table(code_no, con, schema)
  insert_results$table_dimensions <- UMARimportR::insert_new_table_dimensions(
    con, table_dimension_table, schema)
  message("Table dimensions insert: ", insert_results$table_dimensions$count, " rows")
  # prepare and select dimension levels before inserting them
  dimension_levels_table_full <- prepare_dimension_levels_table(code_no, con, schema)
  if(all_levels){
    dimension_levels_table <- dimension_levels_table_full |>
      dplyr::select(-dimension)} else {
  dimension_levels_table <- UMARimportR::dimension_selector(dimension_levels_table_full) |>
    dplyr::select(-dimension)}
  insert_results$dimension_levels <- UMARimportR::insert_new_dimension_levels(
    con, dimension_levels_table, schema)
  message("Dimension levels insert: ", insert_results$dimension_levels$count, " rows")
  # prepare and insert unit table
  unit_table <-  prepare_unit_table(code_no)
  insert_results$units <- UMARimportR::insert_new_unit(con, unit_table, schema)
  message("Units insert: ", insert_results$units$count, " rows")
  # prepare and insert series table
  series_table <- prepare_series_table(code_no, con, schema)
  insert_results$series <- UMARimportR::insert_new_series(con, series_table, schema)
  message("Series insert: ", insert_results$series$count, " rows")
  # prepare and insert series levels table
  series_levels_table <- prepare_series_levels_table(code_no, con, schema)
  insert_results$series_levels <- UMARimportR::insert_new_series_levels(
    con, series_levels_table, schema)
  message("Series levels insert: ", insert_results$series_levels$count, " rows")
  invisible(insert_results)
}

#' Insert data points from SURS
#'
#' Function to prepare and insert SURS data points. The function first prepares
#' the required vintages and inserts them, then prepares the data points
#' table and isnerts it. The function returns the results invisibly.
#'
#' This is a SURS specific function, which should be followed by the generic
#' UMARimportR function to write the vintage hashes and clean up redundant
#' vintages.
#'
#' @param code_no SURS code name of the table
#' @param con Database connection
#' @param schema Schema name
#'
#' @return Insertion results (invisibly)
#' @export
SURS_import_data_points <- function(code_no, con, schema = "platform") {
  message("Importing data points from: ", code_no, " into schema ", schema)
  # collect outputs from the functions into one result list
  result <- list()
  # Try to prepare SURS vintage table but catch any errors
  vintage_result <- tryCatch(
    expr = {list(
      vintages = prepare_vintage_table(code_no, con, schema),
      error = NULL)},
    error = function(e) {
      error_msg <- conditionMessage(e)
      message("Note: ", error_msg)
      return(list(
        vintages = NULL,
        error = error_msg))})
  # Store error message if any
  result$vintage_error <- vintage_result$error
  # Only proceed with import if vintages were prepared successfully
  if (!is.null(vintage_result$vintages)) {
    # import vintages
    result$vintages <- UMARimportR::insert_new_vintage(con, vintage_result$vintages, schema)
    # Prepare data in SURS-specific way
    prep_data <- prepare_surs_data_for_insert(code_no, con, schema)
    # Insert the prepared data
    result$data <- UMARimportR::insert_prepared_data_points(prep_data, con, schema)
  } else {
    message("Skipping import for ", code_no, " due to vintage preparation issue: ", vintage_result$error)
  }
  invisible(result)
}
