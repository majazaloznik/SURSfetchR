#' Parse single row of changes for printing in email
#'
#' This function takes a single 7 column row from the daily changes table output
#' from \link[SURSfetchR]{extract_new_changes} and parses into a nice printable
#' format that can then be used in a reporting email.
#'
#' @param row single row from table output of \link[SURSfetchR]{extract_new_changes}
#'
#' @return a character string in html
#' @export
#'
parse_row <- function(row) {
  date <- format(as.Date(substr(row["veljavnoOd"], 1, 10), format= "%Y-%m-%d"), "%d.%m.%Y")
  text <- row["sporociloSlo"]
  link <- paste0("<a href ='", row["linkSlo"], "'>", row["imeSlo"], "</a>")
  paste("<il>", date, "-", link, "-", text, "</il><br>")
}


#' Prints out changes for a single change type
#'
#' This function takes as its input the daily changes table output
#' from \link[SURSfetchR]{extract_new_changes} and for each type of change (1-6)
#' prints out an emailable text.
#'
#' @param daily table output of \link[SURSfetchR]{extract_new_changes}
#' @param type number from 1 to 6 for each different type of reported change.
#'
#' @return a character string in html
#' @export
#'
print_for_type <- function(daily, type = 2) {
  idNotificationType <- NULL
  ifelse(any(daily$idNotificationType==type),
         paste0("<b>", change_types$opisSlo[type], "</b>:<br><ul>",
               paste(apply(subset(daily, idNotificationType == type) , 1,
                           function(row) parse_row(row)), collapse = ' '), "</ul><br>"), ""
  )
}

#' Email nicely parsed SURS reported changes
#'
#' This email takes the outputs of \link[SURSfetchR]{extract_new_changes} that have
#' been formatted with the helper function \link[SURSfetchR]{print_for_type} and
#' emails it from a hardcoded gmail account - currently mine, but I'll change
#' it to an Umar one. Not included in testing suite.
#'
#' @param daily table output of \link[SURSfetchR]{extract_new_changes}
#' @param recipient single email of recipient
#'
#' @return null - side effect is sending an email.
#' @export
email_surs_changes <- function(daily, recipient = "maja.zaloznik@gmail.com") {
  if(nrow(daily) > 0){
  body <- paste0("To je avtomatsko generirano sporo\\u010dilo. <br>",
                "SURS je na svoji <a href='https://pxweb.stat.si/SiStat/sl/Notifications'>strani</a> objavil naslednje napovedane spremembe:<br><br>",
                paste(unlist(lapply(1:6, function(x) print_for_type(daily, x))), collapse = ' '))

  text_msg <- gmailr::gm_mime() %>%
    gmailr::gm_to(recipient) %>%
    gmailr::gm_subject("Novo-napovedane spremembe na SiStat bazi") %>%
    gmailr::gm_from("maja.zaloznik@gmail.com") %>%
    gmailr::gm_html_body(body)

    gmailr::gm_send_message(text_msg)
  }
}
