#' Prepare table to insert into `table` table
#'
#' This one is really straightforward, and slightly superfluous, since it just
#' uses the \link[SURSfetchR]{get_px_metadata} function and removes two columns.
#' Returns table ready to insert into the `table` table with the db_writing family
#' of functions.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#'
#' @return a dataframe with the `code`, `name`, `source_id`, `url`, and `notes` columns
#' for this table.
#' @export
prepare_table_table <- function(code_no) {
  get_px_metadata(code_no) %>%
    dplyr::select(-updated, -valuenotes, -units)
}

#' Prepare table to insert into `category` table
#'
#' Helper function that extracts all the parent categories from the full
#' hierarchy data.frame, and prepares the category table with field ids and
#' their names. Returns table ready to insert into the `category` table with the db_writing family
#' of functions.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param full full field hierarchy with parent_ids et al, output from
#' \link[SURSfetchR]{get_full_structure}
#' @return a dataframe with the `id`, `name`, `source_id` for each category that
#' the table is a member of.
#' @export
#'
prepare_category_table <- function(code_no, full) {
  id_no <- unique(full$id[full$name == code_no])
  get_row(id_no, full) %>%
    dplyr::select(-parent_id)
}


#' Prepare table to insert into `category_relationship` table
#'
#' Helper function that extracts the field hierarchy from the full
#' hierarchy data.frame, and  prepares the category relationship table with field ids and
#' their parents' id. Returns table ready to insert into the `category_relationship`
#' table with the db_writing family of functions.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param full full field hierarchy with parent_ids et al, output from
#' \link[SURSfetchR]{get_full_structure}
#' @return a dataframe with the `id`, `name`, `parent_id`, `source_id` for each relationship
#' betweeh categories
#' @export
#'
prepare_category_relationship_table <- function(code_no, full) {
  id_no <- unique(full$id[full$name == code_no])
  get_row(id_no, full) %>%
    dplyr::mutate(parent_id = as.numeric(parent_id)) %>%
    dplyr::arrange(parent_id) %>%
    dplyr::select(-name)
}


#' Prepare table to insert into `category_table` table
#'
#' Helper function that extracts the parent category for each table from the full
#' hierarchy data.frame, and fills up the category_table table with the table ids and
#' their categories (parents). A single table can have multiple parents - meaning
#' it is member of several categories (usually no more than two tho). Returns table
#' ready to insert into the `category_table`table with the db_writing family of functions.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @param full full field hierarchy with parent_ids et al, output from
#' \link[SURSfetchR]{get_full_structure}
#' @return a dataframe with the `category_id` `table_id` and `source_id` columns for
#' each table-category relationship.
#' @export
#'
prepare_category_table_table <- function(code_no, full, con) {
  get_table_id(code_no, con) -> tbl_id
  full %>%
    dplyr::filter(name == code_no) %>%
    dplyr::select(category_id = parent_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(table_id = tbl_id,
                  source_id = 1)
}

#' Prepare table to insert into `table_dimensions` table
#'
#' Helper function that extracts the dimensions for each table and their "time" status.
#' Returns table ready to insert into the `table_dimensions`table with the
#' db_writing family of functions.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @return a dataframe with the `table_id`, `dimension_name`, `time` columns for
#' each dimension of this table.
#' @export
#'
prepare_table_dimensions_table <- function(code_no, con) {
  get_table_id(code_no, con) -> tbl_id
  get_table_levels(code_no) %>%
    dplyr::mutate(table_id = tbl_id) %>%
    dplyr::select(table_id, dimension_name, time) %>%
    dplyr::rename(is_time = time,
                  dimension = dimension_name)
}

#' Prepare table to insert into `dimension_levels` table
#'
#' Helper function that extracts the levels for all the dimensions for each
#' table and get their codes and text.
#' Returns table ready to insert into the `dimension_levels`table with the
#' db_writing family of functions.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @return a dataframe with the `dimension_name`, `values`, `valueTexts`, and `id`
#' columns for this table.
#' @export
#'
prepare_dimension_levels_table <- function(code_no, con) {
  get_table_id(code_no, con) -> tbl_id
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::filter(is_time == FALSE) %>%
    dplyr::select(dimension, id) %>%
    dplyr::collect() -> dim_ids

  get_table_levels(code_no) %>%
    tidyr::unnest(levels) %>%
    dplyr::mutate(table_id = tbl_id) %>%
    dplyr::select(dimension_name, values, valueTexts) %>%
    dplyr::inner_join(dim_ids, by = c("dimension_name" = "dimension")) %>%
    dplyr::select(-dimension_name) %>%
    dplyr::rename(tab_dim_id = id,
                  level_value = values,
                  level_text = valueTexts)
}


#' Prepare table to insert into `units` table
#'
#' Helper function that extracts the units for each table from the px metadata.
#' Returns table ready to insert into the `units`table with the
#' db_writing family of functions.
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @return a dataframe with the single column containing the different units used
#' in this table.
#' @export
#'
prepare_units_table <- function(code_no, con) {
data.frame(strsplit(get_px_metadata(code_no)$units, ", ")) %>%
    dplyr::mutate_all(tolower)
}



#' Prepare table to insert into `series` table
#'
#' This is a big one. Prepares the table to insert into the series table, which
#' along expanding the levels to get all the series and their codes as well, which
#' include also figuring out their time interval, this function also tries to
#' extract the unit for each series, which can get pretty tricky. There are generally
#' three options:
#' 1. either the whole table and therefore all the series within it have a single unit
#' which we get from the .px metadata.
#' 2. or there is a MERITVE dimensions, which might (?) contain the units for each
#' level in parentheses at the end of the label. So we get if from there
#' 3. ooor, there is no MERITVE dimension, and then there is (?) a slot in the .px
#' data called `valuenotes`, which contains the units and again there is an even
#' more convoluted way to get them out and know which level values they apply to.
#' Returns table ready to insert into the `series`table with the
#' db_writing family of functions.
#'
#' @param code_no character obect of the matrix code (e.g. 2300123S)
#' @param con connection to the database
#'
#' @return a dataframe with the following columns: `series_title`, `series_code`,
#' `unit_id`, `table_id` and `interval_id`for each series in the table
#' well as the same number of rows as there are series
#' @export

prepare_series_table <- function(code_no, con){
  get_table_id(code_no, con) -> tbl_id
  get_time_dimension(code_no, con) -> time_dimension
  get_interval_id(time_dimension) -> interval_id
  get_single_unit_from_px(code_no, con) -> unit_id
  expand_to_level_codes(code_no, con) -> expanded_level_codes
  expanded_level_codes %>%
    dplyr::mutate(unit_id = unit_id) -> expanded_level_codes
  if(is.na(unit_id)) {
    get_meritve_id(tbl_id, con) -> meritve_dim_id
    get_meritve_no(tbl_id, con) -> meritve_dim_no
    if(length(meritve_dim_id) == 1) {# if MERITVE EXIST
      get_level_text_from_meritve(meritve_dim_id, con) -> level_text_from_meritve
      get_unit_levels_from_meritve(level_text_from_meritve, con) -> units_by_meritve_levels
      add_meritve_level_units(expanded_level_codes, meritve_dim_no,
                              units_by_meritve_levels) -> expanded_level_codes
    } else { # if valuenotes exist
      get_valuenotes_from_px(code_no, tbl_id, con) -> units_by_levels
      get_valuenotes_id(tbl_id, units_by_levels$dim_name[1], con) -> valuenotes_dim_id
      get_valuenotes_no(tbl_id,  units_by_levels$dim_name[1], con) -> valuenotes_dim_no
      if(length(valuenotes_dim_id) == 1){
        add_valuenotes_level_units(expanded_level_codes, valuenotes_dim_no,
                                   units_by_levels) -> expanded_level_codes}
    }
  }

  expanded_level_codes %>%
    tidyr::unite("series_code", dplyr::starts_with("Var"), sep = "--") %>%
    dplyr::mutate(series_code = paste0("SURS--", code_no, "--", series_code, "--",interval_id)) %>%
    cbind(expand_to_series_titles(code_no, con)) %>%
    dplyr::mutate(table_id = tbl_id,
                  interval_id = interval_id)
}



#' Prepare table to insert into `series_levels` table
#'
#' Helper function that extracts the individual levels for each series and
#' gets the correct dimension id for each one and the correct series id to
#' keep with the constraints.
#' Returns table ready to insert into the `series_levels`table with the
#' db_writing family of functions.
#'
#'
#' @param code_no the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @return a dataframe with the `series_id`, `tab_dim_id`, `value` columns
#' all the series-level combinatins for this table.
#' @export
#'
prepare_series_levels_table <- function(code_no, con) {
  get_table_id(code_no, con) -> tbl_id
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  is_time != TRUE) %>%
    dplyr::pull(id) -> dimz

  dplyr::tbl(con, "series") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::collect() %>%
    dplyr::select(table_id, id, code) %>%
    tidyr::separate(code, into = c("x1", "x2", paste0(dimz), "x3"), sep = "--") %>%
    dplyr::select(series_id = id,  paste0(dimz)) %>%
    tidyr::pivot_longer(-series_id, names_to = "tab_dim_id" )
}
