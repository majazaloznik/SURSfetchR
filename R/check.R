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


#' Get complete list of published changes from Notificaitons API
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



#' Compare newly parsed table with old table to see changes in Notifications API
#'
#' A function to extract the newly published changes on the Notificaitons API since
#' the script was previously run.
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
#' Take the new changes extracted from the Notifications API by comparing with
#' the change table, add today's date and append to previous change table.
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

#' Extract changes that are due tomorrow
#'
#' Get dataframe of changes that are coming into effect on tomorrow's date - or
#' another specified date.
#'
#' @param new_df dataframe output from \link[SURSfetchR]{surs_change_api}
#' @param date desired single date, defaults to tomorrow.
#'
#' @return dataframe with 7 columns (no publication date, because we don't need here)
#' @export
extract_tomorrows_changes <- function(new_df, date = Sys.Date() + 1) {
  veljavnoOd <- NULL
 new_df %>%
    dplyr::mutate(veljavnoOd = format(substr(veljavnoOd, 1, 10), format= "%Y-%m-%d")) %>%
    dplyr::filter(veljavnoOd == date)
}


