#' GET the metadata for an individual table from the SURS API
#'
#' Takes the matrix code of a SURS table (with or without the .px extension)
#' and uses GET to return the metadata as a tibble.
#'
#' @param id character vector of length 1 with code of matrix. Can be with or
#' without the .px extension.
#'
#' @return A tibble with four columns and the same number of rows as there
#' are dimensions in the table
#'
#' @export
get_surs_metadata <- function(id) {
  checkmate::qassert(id, "S[5,11]")
  id <- sub(".PX$", "", id)
  id <- sub(".px$", "", id)
  url <- paste0("https://pxweb.stat.si/SiStatData/api/v1/sl/Data/", id, ".px")
  res <-httr::GET(url)
  mtd <- pxweb::pxweb_parse_response(res)
  mtdt_tbl <- tibble::tibble(
    dimension_name = sapply(mtd$variables, function(x) x$text),
    elim = sapply(mtd$variables, function(x) x$elimination),
    time = sapply(mtd$variables, function(x) x$time),
    levels = lapply(mtd$variables, function(x) tibble::as_tibble(x[3:4])))
  #Sys.sleep(0.075)
  return(mtdt_tbl)
}


#' GET metadata and join into dataframe of matrices
#'
#' Starting out with a data frame with an `id` column of matrix names, the
#' function loops through them and parses the GET response metadata into
#' a tibble that is saved in the `levelz` list-column. Uses
#' \link[SURSfetchR]{get_surs_metadata} in the loop. Also pulls out some summary
#' data from the tibbles to the highest levels, like `elim_any` if any dimensions
#' have the elimination flag on, `time_any` if any have the time flag on, `dimz`
#' to get the number of dimensions and `dim_names` with a vector of the dimension
#' names.
#'
#' @param df dataframe with an id column containing the .px codes
#'
#' @return a dataframe appended with the listcolumn of metadata and
#' other summary columns
#' @export
fill_listcolumn_w_mtdt <- function(df) {
  checkmate::assert_names(names(df), must.include = c("id"))
  checkmate::assert_data_frame(df)
  levelz <- vector("list", nrow(df))
  for(i in 1:nrow(df)) {
    levelz[[i]] <- get_surs_metadata(df$id[i])
  }
  df$levelz <- levelz
  df$elim_any <- apply(df, 1, \(x) any(x$levelz$elim == TRUE))
  df$time_any <- apply(df, 1, \(x) any(x$levelz$time == TRUE))
  df$dimz <- apply(df, 1, \(x) nrow(x$levelz))
  df$dim_names <- apply(df, 1, \(x) paste0(x$levelz$dimension_name, collapse = "; "))
 return(df)
}



#' Join together matrix hierarchy with relevant levels - for subset
#'
#' This function takes the matrix hierarchy you get out of \link[SURSfetchR]{get_matrix_hierarchy}
#' and - gets the levels for each of the tables from the API, or rather for
#' a subset if it is given. Outputs a multilevel nested dataframe that makes my head
#' hurt, but has all the dataz.
#'
#' So if you leave all three parameters empty, you will get the full table for all
#'
#' @param mat_h Nested dataframe output of \link[SURSfetchR]{get_matrix_hierarchy}. If not
#' provided the full one will be recreated from the GetStructure API.
#' @param lev_h Nested dataframe output of \link[SURSfetchR]{fill_listcolumn_w_mtdt} with
#' all the relevant levels, but will be requested if not provided.
#' @param subset dataframe with an id column containing the .px codes of matrices of interest.
#' If not provided, full mat_h list is used.
#'
#' @return a 11 column df with fiends, matrixes and levels for all the ids in the subset.
#' @export
#'
matrix_n_level_hierarchy <- function(mat_h = NULL, lev_h = NULL, subset = NULL) {
  if(is.null(subset)) subset <- data.frame(id = unique(mat_h$name))
  if(!c("id") %in% names(subset)) stop("You need to provide the ids of the subset.")
  if(is.null(mat_h)) {
    cont <- get_API_response()
    tree <- parse_structAPI_response(cont)
    full <- get_full_structure(tree)
    mat_h <- get_matrix_hierarchy(full)}
  if(is.null(lev_h)) {
    lev_h <- fill_listcolumn_w_mtdt(subset)
  } else {
    lev_h <- dplyr::right_join(subset, by = c("name" = "id"))
  }
  mat_h %>%
    dplyr::right_join(lev_h, by = c("name" = "id")) -> out
}


