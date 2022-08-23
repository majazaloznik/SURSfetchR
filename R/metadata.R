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
#' @return A data.frame with seven columns and single row
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
               notes = I(list(c(l$NOTE, l$NOTEX)))) %>%
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




