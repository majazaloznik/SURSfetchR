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

