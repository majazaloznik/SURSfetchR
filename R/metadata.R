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
                  url = paste0("https://pxweb.stat.si/SiStatData/api/v1/sl/Data/", code, ".px"),)
  df
}
