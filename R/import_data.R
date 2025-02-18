#' Prepare table to insert into `vintage` table
#'
#' Helper function that populates the vintage table with the new vintages. It gets
#' the series id's in two ways: getting all the series ids for a single table, but
#' also by expanding the non-time dimensions and getting all the codes for the
#' series and looking them up like that. This way we can catch if the dimensions
#' of the tables have changed in any way, and they are in fact different series. In
#' such a case the function doesn't return a table, but a descriptive error.
#'
#'
#' Returns table ready to insert into the `vintage`table with the
#' db_writing family of functions.
#'
#'
#' @inheritParams common_parameters
#'
#' @return a dataframe with the `series_id` and `published` columns
#' for all the series in this table.
#' @export
#'

prepare_vintage_table <- function(code_no, con, schema){
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, code_no, schema)

  published <- get_px_metadata(code_no)$updated
  last_published <- UMARaccessR::sql_get_last_publication_date_from_table_id(tbl_id, con, schema)
  if(identical(published, last_published)) {
    stop(paste0("These vintages for table ", code_no,
                "are not new, they will not be inserted again."))
  } else {
    time_dimension <- UMARaccessR::sql_get_time_dimension_from_table_code(code_no, con, schema)
    interval_id <- get_interval_id(time_dimension)
    expanded_level_codes <- expand_to_level_codes(tbl_id, con, schema)
    expanded_level_codes %>%
      tidyr::unite("series_code", dplyr::starts_with("Var"), sep = "--") %>%
      dplyr::mutate(series_code = paste0("SURS--", code_no, "--",
                                         series_code, "--",interval_id)) -> x
    series_ids <- UMARaccessR::sql_get_series_id_from_series_code(x$series_code, con, schema)
    double_check <- UMARaccessR::sql_get_series_ids_from_table_id(tbl_id, con, schema)
    if(isTRUE(all.equal(sort(series_ids$id), sort(double_check$id)))){
      data.frame(series_id = series_ids,
                 published = published) } else {
                   warning(paste("The newly published data in table", code_no,
                         "seems to have a different structure to the series already",
                         "in the database. The vintages were not imported, update",
                         "the series table first."))}
  }
}



#' Get and prepare data for import
#'
#' Downloads and prepares the timeseries data for importing into the database.
#' Because the original data format has the full labels of the levels, these
#' are recoded into the alphanumeric codes.
#'
#' @inheritParams common_parameters
#'
#' @return a dataframe the same size as was downloaded from the .px file, but
#' with recoded dimension levels.
#' @export
prepare_data_table <- function(code_no, con, schema){
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, code_no, schema)
  time_dim <- UMARaccessR::sql_get_time_dimension_from_table_code(code_no, con, schema)
  px <- get_px_data(code_no)
  df <- px[[1]]

  # remove time dimension from lists
  px[[2]][[`time_dim`]] <- NULL
  labels <- px[[2]]
  px[[3]][[`time_dim`]] <- NULL
  codes <- px[[3]]

  non_time_dims <- names(codes)
  # map recoding on list of non/time dimensions and join together.
  purrr::map(seq(length(non_time_dims)),
             ~recode_labels(.x, codes, labels, df)) %>%
    purrr::reduce(cbind) %>%
    dplyr::select(unique(colnames(.)))
}
