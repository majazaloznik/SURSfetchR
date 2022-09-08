#' Get the metadata for an individual table
#'
#' In addition to the  \link[SURSfetchR]{get_table_levels} function, which gets the table's
#' dimensions and levels, this one gets some other metadata from the .px file, which are
#' not available to the pxweb library but use the pxR library. These are the creation date,
#' units and notes, which are parsed as json and some other minor things.
#'
#' @param id character vector of length 1 with code of matrix. Can be with or
#' without the .px extension.
#'
#' @return A data.frame with eight columns and single row
#' @export
#'
get_px_metadata <- function(id) {
  checkmate::qassert(id, "S[5,11]")
  id <- sub(".PX$", "", id)
  id <- sub(".px$", "", id)
  url <- paste0("https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/", id, ".px")
  l <- pxR::read.px(url,
                    encoding = "CP1250",
                    na.strings = c('"."', '".."', '"..."', '"...."')
  )
  df <- data.frame(code = unlist(l$MATRIX),
                   name = unlist(l$DESCRIPTION),
                   created = as.POSIXct(l$CREATION.DATE[[1]],format="%Y%m%d %H:%M",tz=Sys.timezone()),
                   units = l$UNITS[[1]],
                   notes = I(list(c(l$NOTE, l$NOTEX))),
                   valuenotes =I(list(l$VALUENOTE))) %>%
    dplyr::mutate(notes = jsonlite::toJSON(notes),
                  source = 1,
                  url = paste0("https://pxweb.stat.si/SiStatData/api/v1/sl/Data/", code, ".px"))
  df
}
#' Helper function to get parent category from full hierarchy
#'
#' This is a recursive function to be used inside another semi-recursive
#' function \link[SURSfetchR]{get_row} to extract the ids of categories and
#' their parents.
#'
#' @param id_no id of category looking for its parent
#' @param full full hierarchy of matrixes and fields out of
#' \link[SURSfetchR]{get_full_structure}.
#' @param row subset of full with relevant ids passed from \link[SURSfetchR]{get_row}
#' @param output output table
#'
#' @return dataframe with four columns with id, name, parent id and source id (which is
#' 1 for SURS)
#' @export

get_parent <- function(id_no, full, row, output) {
  parent <- row$parent_id
  if(is.na(row$matrix_name)) {
    output <- dplyr::bind_rows(output,c(id = id_no,
                                             name = row$name,
                                             parent_id = parent,
                                             source_id = 1))}
  if(!parent == 0) {
    full %>%
      dplyr::filter(id == parent) -> row
    if (nrow(row) == 1) {
      output <- get_parent(parent, full, row, output)
     } else {
       output <- get_row(parent, full, output)}}
  return(output)
}



#' Semi-recursive function to get out the category hierarchy
#'
#' Using the recursive \link[SURSfetchR]{get_parent} in a weird way that works
#' in most cases - but doensn't work that great when a category has two parents,
#' which on occasion happens. Anyway, this is solved using distinct, so the
#' results are fine, just the internals are not as efficient as they could be
#' if i was a bit smarter.
#'
#' @param id_no id of category looking for its parent
#' @param full full hierarchy of matrixes and fields out of
#' \link[SURSfetchR]{get_full_structure}.
#' @param output output table
#'
#' @return  dataframe with four columns with id, name, parent id and source id (which is
#' 1 for SURS), with unique rows, ready to be inserted into the database.
#' @export
get_row <- function(id_no, full, output = NULL){
  checkmate::assert_names(names(full), must.include = c("matrix_name",
                                               "parent_id",
                                               "id"))
  if(is.null(output)){
    output <- data.frame(id = character(),
                         name = character(),
                         parent_id = character(),
                         source_id = character())}
  full %>%
    dplyr::filter(id == id_no) -> rows
  for (row in seq_len(nrow(rows))){
    output <- get_parent(id_no, full, rows[row,], output)
  }
  output %>%
    dplyr::distinct() -> output
  return(output)
}


#' Clean up specific metadata from the px file
#'
#' Helper functions for specific slots in the output from
#' \link[SURSfetchR]{get_px_metadata} that extracts and makes useable the stuff
#' in there
#' @param code_no character string of px code
#' @param tbl_id numeric value of table's id in the `table` table
#' @rdname get_px_stuff
#' @keywords internal
get_single_unit_from_px <- function(code_no){
  units_from_px <- unlist(strsplit(get_px_metadata(code_no)$units, ", "))
  if(length(units_from_px)==1) {
    unit_id <- get_unit_id(units_from_px)} else {
      unit_id <- NA}
  unit_id
}
#' @rdname get_px_stuff
#' @keywords internal
get_valuenotes_from_px <- function(code_no, tbl_id) {
  as_tibble(get_px_metadata(code_no)$valuenotes[[1]]) %>%
    tidyr::gather() -> x
  if(nrow(x)>0){
  purrr::map_dfr(x$key, ~ c(dim_name = get_valuenotes_dimension(.),
                     level_text = get_valuenotes_level(.))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(tab_dim_id = get_tab_dim_id(tbl_id, dim_name),
           level_value = get_level_value(tab_dim_id, level_text)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(unit_id = purrr::map_dbl(x$value, get_valuenotes_unit, con)) -> out} else {
      out <- NULL}
}


#' Family of helper functions to extract units from VALUENOTES list
#'
#' Some SURS matrices have the units for individual levels saved in a list
#' in the VALUENOTES slot of the px. metadata. These need to be regexed outta there
#' with the following three functions.
#'
#' @param x a character string, either the element name (for dimension and level)
#' or the element itself (for the unit)
#' @return character string of cleaned up dimension name, dimension level label or
#' unit name (which is what we are after in the end).
#' @rdname valuenotes
#' @keywords internal
get_valuenotes_dimension <- function(x){
  x <- regmatches(x, regexpr("[A-Z.]+(?=\\.)", x, perl = TRUE))
  gsub( "\\.", " ", x)
}

#' @rdname valuenotes
#' @keywords internal
get_valuenotes_level <- function(x){
  y <- gsub( "\\.", " ", x)
  gsub(paste0(get_valuenotes_dimension(x), " "), "", y)
}

#' @rdname valuenotes
#' @keywords internal
get_valuenotes_unit <- function(x){
  y <- regmatches(x, regexpr("(?<=Enota: ).+", x, perl = TRUE))
  y <- gsub( "\\.", "", y)
  unit_name <- gsub("\" \"", "", y)
  as.numeric(get_unit_id(unit_name))
}
