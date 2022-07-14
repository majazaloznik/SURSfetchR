#' Parse single row of changes for printing in email
#'
#' This function takes a single 7 column row from the daily changes table output
#' from \link[SURSfetchR]{extract_new_changes} and parses into a nice printable
#' format that can then be used in a reporting email. Helper function used in
#' \link[SURSfetchR]{print_for_type}.
#'
#' @param row single row from table output of \link[SURSfetchR]{extract_new_changes}
#'
#' @return a character string in html
#' @keywords internal
parse_row <- function(row) {
  date <- format(as.Date(substr(row["veljavnoOd"], 1, 10), format= "%Y-%m-%d"), "%d.%m.%Y")
  text <- row["sporociloSlo"]
  link <- paste0("<a href ='", row["linkSlo"], "'>", row["imeSlo"], "</a>")
  paste("<il>", date, "-", link, "-", text, "</il><br>")
}


#' Prints out changes for a single change type from Notifications
#'
#' This function takes as its input the Notifications daily changes table output
#' from \link[SURSfetchR]{extract_new_changes} or \link[SURSfetchR]{extract_tomorrows_changes}
#' and for each type of change (1-6) prints out an emailable text.
#'
#' @param daily table output of \link[SURSfetchR]{extract_new_changes}
#' @param type number from 1 to 6 for each different type of reported change.
#'
#' @return a character string in html
#' @keywords internal
print_for_type <- function(daily, type = 2) {
  idNotificationType <- NULL
  ifelse(any(daily$idNotificationType==type),
         paste0("<i>", change_types$opisSlo[type], "</i>:<br><ul>",
               paste(apply(subset(daily, idNotificationType == type) , 1,
                           function(row) parse_row(row)), collapse = ' '), "</ul><br>"), ""
  )
}


#' Prepare html body to email SURS Notifications changes
#'
#' This function takes the outputs of \link[SURSfetchR]{extract_new_changes} and
#' \link[SURSfetchR]{extract_tomorrows_changes} that have
#' been formatted with the helper function \link[SURSfetchR]{print_for_type} and
#' prepares the html body for emailing
#'
#' @param changes table output of \link[SURSfetchR]{extract_new_changes}
#' @param today table output of \link[SURSfetchR]{extract_tomorrows_changes}
#'
#' @return chr.string or empty string
#' @export
email_surs_changes_body <- function(changes, today) {
  if(nrow(changes) > 0 | nrow(today) > 0){
    email_body <- "To je avtomatsko generirano sporo\u010dilo o napovedanih strukturnih spremembah v podatkovni bazi SiStat, ki jih SURS objavlja <a href='https://pxweb.stat.si/SiStat/sl/Notifications'>tukaj</a>.<br><br>"
    if(nrow(changes) > 0) {
      email_body <- paste0(email_body,
                     "<b>SURS je na novo objavil naslednje napovedane spremembe:</b><br><br>",
                     paste(unlist(lapply(1:6, function(x) print_for_type(changes, x))), collapse = ' '), "<br><br>")
    }
    if(nrow(today) > 0) {
      email_body <- paste0(email_body,
                     "<b>Jutri za\u010dnejo veljati naslednje spremembe:</b><br><br>",
                     paste(unlist(lapply(1:6, function(x) print_for_type(today, x))), collapse = ' '))
    }
    email_body
  }
}


#' Email nicely parsed SURS reported Notifications changes
#'
#' This function takes a html text as the body from  \link[SURSfetchR]{email_surs_changes_body}
#' and emails it from a hardcoded gmail account - the Umar Data Bot one one.
#' Not included in testing suite. This will only work with the OAuth secrets in
#' the gm_autn() setup from wherever this function is run.
#'
#' @param body html character string output of  \link[SURSfetchR]{email_surs_changes_body}
#' @param recipient single email of recipient
#'
#' @return null - side effect is sending an email.
#' @export
email_surs_changes <- function(body, recipient = "maja.zaloznik@gmail.com") {
  if(!is.null(body)){
    text_msg <- gmailr::gm_mime() %>%
      gmailr::gm_to(recipient) %>%
      gmailr::gm_subject("Spremembe na SiStat bazi") %>%
      gmailr::gm_html_body(body)

    gmailr::gm_send_message(text_msg)
  }
}
