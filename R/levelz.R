#' GET the metadata for an individual table from the SURS API
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
  id <- sub(".PX$", ".px", id)
  id <- ifelse(grepl(".px$", id), id, paste0(id, ".px"))
  url <- paste0("https://pxweb.stat.si/SiStatData/api/v1/sl/Data/", id)
  res <-httr::GET(url)
  mtd <- pxweb:::pxweb_parse_response(res)
  mtdt_tbl <- tibble::tibble(
    dimension_name = sapply(mtd$variables, function(x) x$text),
    elim = sapply(mtd$variables, function(x) x$elimination),
    time = sapply(mtd$variables, function(x) x$time),
    levels = lapply(mtd$variables, function(x) tibble::as_tibble(x[3:4])))
  return(mtdt_tbl)
}
#
#
# fill_listcolumn_w_mtdt(df){
#
#  return(df)
# }
