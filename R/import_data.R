#' Prepare table to insert into `vintage` table
#'
#' Helper function that populated the vintage table with the new vintages. It gets
#' the series id's in two ways: getting all the series ids for a single table, but
#' also by expanding the non-time dimensions and getting all the codes for the
#' series and looking them up like that. This way we can catch if the dimensions
#' of the tables have changed in any way, and they are in fact different series. In
#' such a case the function doens't return a table, but a descriptive error.
#'
#' extracts the individual levels for each series and
#' gets the correct dimension id for each one and the correct series id to
#' keep with the constraints.
#'
#' Returns table ready to insert into the `vintage`table with the
#' db_writing family of functions.
#'
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @return a dataframe with the `series_id` and `published` columns
#' for all the series in this table.
#' @export
#'

prepare_vintage_table <- function(code_no, con){
  get_table_id(code_no, con) -> tbl_id
  get_time_dimension(code_no, con) -> time_dimension
  get_interval_id(time_dimension) -> interval_id
  expand_to_level_codes(code_no, con) -> expanded_level_codes
  expanded_level_codes %>%
    tidyr::unite("series_code", dplyr::starts_with("Var"), sep = "--") %>%
    dplyr::mutate(series_code = paste0("SURS--", code_no, "--",
                                       series_code, "--",interval_id)) -> x
  get_series_id(x$series_code, con) -> series_ids
  get_series_id_from_table(tbl_id, con) -> double_check
  if(all.equal(series_ids, double_check)){
    get_px_metadata(code_no)$created -> published
    data.frame(series_id = series_ids,
               published = published) } else {
                 stop(paste("The newly published data in table", code_no,
                       "seems to have a different structure to the series already",
                       "in the database. The vintages were not imported, update",
                       "the series table first.")) }
}
