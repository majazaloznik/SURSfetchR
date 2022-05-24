#' Get the codes of updated tables
#'
#' This function parses the html list on the SURS website which seems to be the
#' only outside facing endpoint where you can get access to the update date
#' for each of their published tables. And no, the API doesn't have this info.
#'
#' [This](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/) is the table and we
#' want the date from the alt text on the little clock icon and the id of the
#' which is in the link.
#'
#' @return a dataframe with two columns and over 3700 rows with the id of the
#' table and the date it was last updated.
#'
#' @export
#'
parse_surs_updates <- function() {
  # link to webpage with date info
  url <- "https://pxweb.stat.si/SiStatData/pxweb/sl/Data/"
  #read the HTML contents
  content_URL <- rvest::read_html(url)

  # identify urls (get the xpaths from selectorgadget.com)
  xpath_links <- '//*[contains(concat( " ", @class, " " ), concat( " ", "AspNet-TreeView-Root", " " ))]//a'
  anchors_hrefs <- rvest::html_nodes(content_URL, xpath = xpath_links)
  links <- rvest::html_attr(anchors_hrefs, 'href')
  # regex this baby
  ids <- gsub("/", "",gsub("/SiStatData/pxweb/sl/Data/-/", "", links))

  #identify the anchor tags in the first page URL
  xpath_icons <- '//*[contains(concat( " ", @class, " " ), concat( " ", "tableofcontent_metaicon", " " ))]'
  anchors_icons <- rvest::html_nodes(content_URL, xpath = xpath_icons)
  dates <- rvest::html_attr(anchors_icons, 'title')
  dates <- dates[seq(2,length(dates), 3)]
  # regex this baby
  dates <- gsub("Spremenjeno: ", "", dates)
  # create df
  data.frame(id = ids, date_updated = as.Date(dates, format = "%d. %m. %Y"))
}


#' Subset parsed df by date updated
#'
#' @param df output of \link[SURSfetchR]{parse_surs_updates}
#'
#' @param date date to subset the table by, if empty defaults to today's date,
#' if NULL returns whole table.
#'
#' @return table dataframe with two columns, nrow depends on subset.
#' @export
#'
subset_parsed_df <- function(df, date = Sys.Date()){
  date_updated <- NULL
  if (is.null(date)) {df} else {
    df %>%
      dplyr::filter(date_updated == date)
  }
}


#' Get daily table changes from SURS' OPSI API
#'
#' SURS set up a convenience mini API for OPSI so they could get info on the
#' daily changes. It returns New, Updated and Deleted tables. This function
#' gets the response from the API for a particular date and desired table and
#' cleans it up to keep just the date, clean table id code and title (the latter
#' is just for kicks).
#'
#' @param date required, defaults to today.
#' @param table One of "New", "Update" or "Delete", defaulting to "Update"
#'
#' @return a dataframe with three columns, or an error message if there was no data.
#' @export
#'
surs_opsi_api <- function(date = Sys.Date(), table = "Update") {
  tryCatch(
    { url <- paste0("https://pxweb.stat.si/SiStat/sl/Api/", table)
    date <- date

    body <- paste0("{'Date': '", date, "'}")

    r <- httr::POST(url= url,
              body = body,
              httr::content_type("application/json"))

    df <- data.frame(matrix(unlist(httr::content(r)), nrow=length(httr::content(r)), byrow=TRUE)) %>%
      dplyr::select(1:3)

    return(df)
    },
    error = function(error_message) {
      message("Looks like there is no data to return. Here's the original error:")
      message(error_message, "\n")
      return(NA)
    }
  )
}

