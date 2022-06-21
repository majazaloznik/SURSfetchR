# # setting up the "original" table - the one to start with. 20.6.2022
# library(dplyr)
# url <- paste0("https://pxweb.stat.si/SiStat/sl/Api/GetNotifications")
#
# request <- httr::GET(url= url,
#                      httr::content_type("application/json"))
#
# parsed_request <- jsonlite::fromJSON(httr::content(request, as = "text"))
#
# parsed_request[[3]] %>%
#   tidyr::unnest(podrocja) %>%
#   dplyr::left_join(parsed_request[[2]]) %>%
#   dplyr::select(-dplyr::ends_with("Ang")) -> test_request
#
# test_request %>%
#   filter(!(grepl("1817607S", sporociloSlo) | grepl("05L2012S", sporociloSlo)
#            | grepl("05J1027S", sporociloSlo))) %>%
#   mutate(verjetniDatumObjave = as.Date("1970-01-01", format = "%Y-%m-%d")) -> original
#
# saveRDS(original, "M:/analysis/SURSfetchR/tests/testdata/original.rds")
# saveRDS(test_request, "M:/analysis/SURSfetchR/tests/testdata/test_request.rds")

# codelist of different types of changes reported by the SURS change API
url <- paste0("https://pxweb.stat.si/SiStat/sl/Api/GetNotifications")

request <- httr::GET(url= url,
                     httr::content_type("application/json"))

parsed_request <- jsonlite::fromJSON(httr::content(request, as = "text"))
change_types <- parsed_request[[2]]



################################################################################
# rerun this after you make any changes here!!!
################################################################################
usethis::use_data(change_types,
                  internal = TRUE, overwrite = TRUE)
