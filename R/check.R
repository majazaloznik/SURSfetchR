#' Get the codes of updated tables
#'
#' This function parses the html list on the SURS website which seems to be the
#' only outside facing endpoint where you can get access to the update date
#' for each of their published tables. And no, the API doesn't have this info.
#'
#' [This](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/) is the table and we
#' want the date from the alt text on the little clock icon (the date of the
#' most recent change) and the id of the table, which is in the link.
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


#' Get complete list of published changes
#'
#' SURS set up another convenience API with the data used for
#' [this list](https://pxweb.stat.si/SiStat/en/Notifications) where they publish
#' all the anticipated changes to the database structure. This funciton pulls the
#' data and parses it into a neat dataframe with only relevant (slovenian) columns.
#'
#' @param body body to pass to the GET request, but really only required for
#' testing, because there is no parameters required on this request.
#' @return a data frame with 7 columns
#' @export
#'
surs_change_api <- function(body = NULL) {
  tryCatch(
    {url <- paste0("https://pxweb.stat.si/SiStat/sl/Api/GetNotifications")
    podrocja <- NULL

    request <- httr::GET(url= url,
                         body = body,
                         httr::content_type("application/json"))

    parsed_request <- jsonlite::fromJSON(httr::content(request, as = "text"))

    suppressMessages(parsed_request[[3]] %>%
      tidyr::unnest(podrocja) %>%
      dplyr::left_join(parsed_request[[2]]) %>%
      dplyr::select(-dplyr::ends_with("Ang")) -> x)

    return(x)
    },
    error = function(error_message) {
      message("Looks like there is no data to return. Here's the original error:")
      message(error_message, "\n")
      return(NA)
    }
  )
}



#' Compare newly parsed table with old table to see changes
#'
#' @param new_df dataframe output from \link[SURSfetchR]{surs_change_api}
#' @param old_df dataframe output from \link[SURSfetchR]{update_change_table}
#'
#' @return dataframe with 7 columns
#' @export
#'
extract_new_changes <- function(new_df, old_df) {
  suppressMessages(new_df %>%
    dplyr::anti_join(old_df) -> changes)
  return(changes)
}


#' Append new changes to change table
#'
#' Take the new changes extracted from the API by comparing with the change table
#' add today's date and append to previous change table
#'
#' @param old_df dataframe output from \link[SURSfetchR]{update_change_table}
#' @param changes dataframe output from \link[SURSfetchR]{extract_new_changes}
#'
#' @return dataframe with 8 columns
#' @export
#'
update_change_table <- function(old_df, changes) {
 changes %>%
    dplyr::mutate(verjetniDatumObjave = Sys.Date()) %>%
    dplyr::bind_rows(old_df)
}

#' Extract changes that are due today
#'
#' Get dataframe of changes that are coming into effect on today's date - or
#' another specified date
#'
#' @param new_df dataframe output from \link[SURSfetchR]{surs_change_api}
#' @param date desired single date, defaults to today.
#'
#' @return dataframe with 7 columns (no publication date, because we don't need here)
#' @export
#'
extract_todays_changes <- function(new_df, date = Sys.Date()) {
  veljavnoOd <- NULL
 new_df %>%
    dplyr::mutate(veljavnoOd = format(substr(veljavnoOd, 1, 10), format= "%Y-%m-%d")) %>%
    dplyr::filter(veljavnoOd == date)
}


