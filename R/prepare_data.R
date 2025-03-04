#' Prepare table to insert into `vintage` table
#'
#' Helper function that populates the vintage table with the new vintages. It gets
#' the series id's from the database and adds the publication date from the px.
#'
#' Returns table ready to insert into the `vintage`table with the
#' UMARimportr::insert family of functions.
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
    series_ids <- UMARaccessR::sql_get_series_ids_from_table_id(tbl_id, con, schema)
    data.frame(series_ids, published) |>
      dplyr::rename(series_id = id)
  }
}



#' Get and prepare data for import
#'
#' Downloads and prepares the timeseries data for importing into the database.
#' Because the original data format has the full labels of the levels, these
#' are recoded into the alphanumeric codes.
#' The datapoints are filtered to only include the series that are in the
#' database in case some levels have been removed during the structure import.
#'
#' @inheritParams common_parameters
#'
#' @return a dataframe that was downloaded from the .px file, but
#' with recoded dimension levels and filtered series we arent' tracking.
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
  df <- purrr::map(seq(length(non_time_dims)),
                   ~recode_labels(.x, codes, labels, df)) %>%
    purrr::reduce(cbind) %>%
    dplyr::select(unique(colnames(.)))
  # remove levels that we are not tracking
  dim_levels <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con, schema)
  dim_list <- split(dim_levels$level_value, dim_levels$dimension)
  names(dim_list)<- gsub(" ", ".", names(dim_list))
  dim_cols <- names(dim_list)
  keep_rows <- rep(TRUE, nrow(df))
  for (col in dim_cols) {
    keep_rows <- keep_rows & df[[col]] %in% dim_list[[col]]
  }
  df[keep_rows, ]
}

#' Prepare SURS data table for insertion
#'
#' Processes raw SURS data into a format ready for database insertion,
#' handling SURS-specific quirks like flags in time periods.
#'
#' @param code_no SURS code name of the table
#' @param con Database connection
#' @param schema Schema name
#'
#' @return A list containing:
#'  - data: The processed data frame
#'  - table_id: The table ID
#'  - time_dimension: The name of the time dimension
#'  - interval_id: The interval ID
#'  - dimension_ids: The non-time dimension IDs
#'  - dimension_names: The names of the dimensions
#' @export
prepare_surs_data_for_insert <- function(code_no, con, schema = "platform") {
  # Get raw data
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
  df <- purrr::map(seq(length(non_time_dims)),
                   ~recode_labels(.x, codes, labels, df)) %>%
    purrr::reduce(cbind) %>%
    dplyr::select(unique(colnames(.)))
  # remove levels that we are not tracking
  dim_levels <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con, schema)
  dim_list <- split(dim_levels$level_value, dim_levels$dimension)
  names(dim_list)<- gsub(" ", ".", names(dim_list))
  dim_cols <- names(dim_list)
  keep_rows <- rep(TRUE, nrow(df))
  for (col in dim_cols) {
    keep_rows <- keep_rows & df[[col]] %in% dim_list[[col]]
  }
  df <- df[keep_rows, ]

  # Clean column names
  names(df) <- gsub("[^\x01-\x7F]+", "", names(df))

  # Get metadata
  dim_ids <- UMARaccessR::sql_get_non_time_dimensions_from_table_id(tbl_id, con, schema)
  interval_id <- get_interval_id(time_dim)

  # Extract time and flags
  time_column <- gsub("[^\x01-\x7F]+", "", time_dim)
  df[[time_column]] <- as.character(df[[time_column]])
  df$raw_time <- df[[time_column]]
  df$time <- sapply(df$raw_time, function(x) {
    parts <- strsplit(x, " ")[[1]]
    parts[1]  # Return first part (the time)
  })

  df$flag <- sapply(df$raw_time, function(x) {
    parts <- strsplit(x, " ")[[1]]
    if (length(parts) > 1) {
      flag_text <- paste(parts[-1], collapse = " ")
      if (grepl("začasni podatki|zacasni podatki", flag_text, ignore.case = TRUE)) {
        return("T")
      }
      return(flag_text)
    }
    return("")
  })
  df$interval_id <- interval_id

  # Return structured result
  list(
    data = df,
    table_id = tbl_id,
    time_dimension = time_dim,
    interval_id = interval_id,
    dimension_ids = dim_ids$id,
    dimension_names = dim_ids$dimension
  )
}
