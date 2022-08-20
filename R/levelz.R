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
    levels = lapply(mtd$variables, function(x) tibble::as_tibble(x[3:4]))) %>%
    dplyr::mutate(id = sub(".px$", "", id))
  return(mtdt_tbl)
}


#' GET metadata and join into dataframe of matrices
#'
#' Starting out with a data frame with an `id` column of matrix names, the
#' function loops through them and parses the GET response metadata into
#' a tibble that is saved in the `levelz` list-column. Uses
#' \link[SURSfetchR]{get_surs_metadata} in the loop.
#'
#' @param df dataframe with an id column containing the .px codes
#'
#' @return a dataframe appended with the listcolumn of metadata
#' @export
fill_listcolumn_w_mtdt <- function(df) {
  checkmate::assert_names(names(df), must.include = c("id"))
  checkmate::assert_data_frame(df)
  levelz <- vector("list", nrow(df))
  for(i in 1:nrow(df)) {
    levelz[[i]] <- get_surs_metadata(df$id[i])
    Sys.sleep(0.1)
  }
  df$levelz <- levelz
 return(df)
}


#' Extract relevant info from the listcolumn with dimensions and levels
#'
#' Taking the nested dataframe output of \link[SURSfetchR]{fill_listcolumn_w_mtdt}, this
#' function extracts some stuff from the listcolumn with the data on dimensions and
#' levels for each row, pulling them out into the top level of the dataframe.  Pulls out some summary
#' data from the tibbles to the highest levels, like `elim_any` if any dimensions
#' have the elimination flag on, `time_any` if any have the time flag on, `dimz`
#' to get the number of dimensions and `dim_names` with a vector of the dimension
#' names.
#'
#' Also changes the data type for the updated to POSIXct
#'
#' @param df dataframe output of \link[SURSfetchR]{fill_listcolumn_w_mtdt}
#'
#' @return a dataframe with six additional columns
#' @export
pull_levels <- function(df){
  df$elim_any <- apply(df, 1, \(x) any(x$levelz$elim == TRUE))
  df$time_any <- apply(df, 1, \(x) any(x$levelz$time == TRUE))
  df$dimz <- apply(df, 1, \(x) nrow(x$levelz))
  df <- df %>%
    dplyr::mutate(dim_names = purrr::map(levelz, ~ .x$dimension_name),
                  dim_lz =   purrr::map(df$levelz, ~  purrr::map_dbl(.x$levels, nrow)))
  df$no_points <- apply(df, 1, \(x) prod(unlist(x$dim_lz)))
  if("updated" %in% colnames(df)) {
  df %>%
    dplyr::mutate(updated_file = as.POSIXct(updated,format="%Y-%m-%dT%H:%M:%S",tz=Sys.timezone()),
                  .keep = "unused") -> df}
  return(df)
}

#' Join together matrix hierarchy with relevant levels - for subset
#'
#' This function takes the matrix hierarchy you get out of \link[SURSfetchR]{get_matrix_hierarchy}
#' and - gets the levels for each of the tables from the API, or rather for
#' a subset if it is given. Outputs a multilevel nested dataframe that makes my head
#' hurt, but has all the dataz.
#'
#' So if you leave all three parameters empty, you will get the full table for all.
#'
#' If both the full `mat_h` and `lev_h` are given, then subset determines which ones are
#' in the final table.
#'
#' `lev_h` also operates as a subset if it is given, but this is really just useful
#' for testing.
#'
#' Also, removes the archive matrices if the (default) archive parameter is set to FALSE.
#'
#' Importantly, the default also removes matrices that don't have a time parameter set
#' which can be changed by changing `time` to `FALSE`.
#'
#'
#' @param mat_h Nested dataframe output of \link[SURSfetchR]{get_matrix_hierarchy}. If not
#' provided the full one will be recreated from the GetStructure API.
#' @param lev_h Nested dataframe output of \link[SURSfetchR]{fill_listcolumn_w_mtdt} with
#' all the relevant levels, but will be requested if not provided.
#' @param subset dataframe with an id column containing the .px codes of matrices of interest.
#' If not provided, full mat_h list is used.
#' @param archive default FALSE, removes rows with archived matrices
#' @param time defaults to TRUE ensuring only matrices with a tagged time dimension
#' are included.
#' @return a 13 column df with fields, matrixes and levels for all the ids in the subset.
#' @export
#'
matrix_n_level_join <- function(mat_h = NULL, lev_h = NULL, subset = NULL,
                                     archive = FALSE, time = TRUE) {
  if(is.null(subset)) subset <- data.frame(id = unique(mat_h$name))
  if(!c("id") %in% names(subset)) stop("You need to provide the ids of the subset.")
  subset$id <- sub(".PX$", "", subset$id)
  subset$id <- sub(".px$", "", subset$id)
  if(is.null(mat_h)) {
    cont <- get_API_response()
    tree <- parse_structAPI_response(cont)
    full <- get_full_structure(tree)
    mat_h <- get_matrix_hierarchy(full)}
  if(isFALSE(archive)) {
      mat_h %>%
      dplyr::filter(!grepl("archiveMatrixList", pathString)) -> mat_h}
  if(is.null(lev_h)) {
    lev_h <- pull_levels(fill_listcolumn_w_mtdt(subset))
  } else {
    if(!c("id") %in% names(lev_h)) stop("You need to provide the ids in the levels table")
    lev_h$id <- sub(".PX$", "", lev_h$id)
    lev_h$id <- sub(".px$", "", lev_h$id)
    lev_h <- lev_h %>% dplyr::inner_join(subset, by = c("id" = "id"))
  }
  mat_h %>%
    dplyr::inner_join(lev_h, by = c("name" = "id")) -> out
  if(time) {
  out %>%
    dplyr::filter(time_any) -> out}
  return(out)
}


#' Extract time dim stuff from nested tables in fully joined matrix & level table
#'
#' With the fully joined table output from \link[SURSfetchR]{matrix_n_level_join},
#' this function extracts some more stuff from the nested listcolumns especially
#' the dimensions that are not time related and calculates the number of
#' series in each table (`no_series`).T
#'
#' This is not foolproof in the sense that the time dimension must be explicitly
#' flagged with the time argument in the header, so if SURS doesn't do that,
#' then the matrix won't show up here
#'
#'
#' @param df dataframe output of \link[SURSfetchR]{matrix_n_level_join}
#'
#' @return dataframe with three more columns than going in: `dim_names_notime` are
#' the dimensions without the time dimension, `dim_lz_notime` are the numbers of
#' levels of these dimensions and `no_series` is the number of series in the matrix
#'
#' @export
#'
full_hierarchy_unnest <- function(df) {
  if(nrow(df) >0) {
    # df$dim_notime <- apply(df, 1, \(x) x$levelz$time)
    df <- df %>%
      dplyr::mutate(dim_notime = purrr::map(levelz, ~ .x$time),
                    dim_names_notime = purrr::map2(dim_names, dim_notime, ~ .x[!.y]),
                    dim_lz_notime = purrr::map2(dim_lz, dim_notime,  ~ .x[!.y]),
                    no_series = purrr::map_dbl(dim_lz_notime, prod)) %>%
      dplyr::select( -dim_notime)}
  return(df)
}


#' Helper function for extracting level combinations for each matrix
#'
#' Starting from the dataframe output from \link[SURSfetchR]{full_hierarchy_unnest}
#' this helper function gets the non-time levels from the `levelz` table, specifically
#' their names and creates a table with all possible combinations i.e. a table listing
#' all the series inside each matrix. Can be used `rowwise` in a `mutate` call with
#' something like  `list(get_level_combos(levelz, dim_names_notime)))`, but don't
#' try running it on the whole table, since it would end up with 415 million rows
#' and that won't work..
#'
#' @param x name of listcolumn with levelz tibbles
#' @param y name of listcolumn with dimension names to be used (so you can use
#' a subset - usually the subset excluding the time variable)
#' @return a dataframe or tibble with a column for each level and a row for each
#' combination of level values.
#
#' @export
get_level_combos <- function(x, y) {
  x %>%
    dplyr::filter(dimension_name %in% unlist(y)) %>%
    dplyr::pull(levels) %>%
    purrr::map( "valueTexts") %>%
    expand.grid()
}
