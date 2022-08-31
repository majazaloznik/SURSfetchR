devtools::install_github("majazaloznik/SURSfetchR", dependencies = FALSE)
library(SURSfetchR)
library(tidyr)
library(dplyr)
library(purrr)

rsp <- get_API_response()
out <- parse_structAPI_response(rsp)
full <- get_full_structure(out)
mat_h <-  get_matrix_hierarchy(full)

api_list <- pxweb::pxweb_get("https://pxweb.stat.si/SiStatData/api/v1/sl/Data")
df <- as.data.frame(api_list)


df_w_mtdt <- readRDS("../../analysis/mesecni_kazalniki/data/matrices_w_levels.rds")


full_hierarchy <- matrix_n_level_join(mat_h, df_w_mtdt, archive = TRUE, time = FALSE)
full_hierarchy <- full_hierarchy_unnest(full_hierarchy)
full_hierarchy %>%
  mutate(check = difftime(updated.x, updated.y, units="hours")) -> x

x <- get_row(12953, full)
get_row(12953, full)
master_list_surs <- readRDS("M:/analysis/mesecni_kazalniki/data/master_list_surs.rds")
full %>%
  dplyr::filter(name %in% master_list_surs$code) -> x



insert_table <- data.frame(table = character(),
                           sql = character())

insert_table <- dplyr::bind_rows(insert_table,
                          c(table = "table",
                            sql = paste("INSERT INTO \"table\"",
                                        "(code, name, source_id, url, description, notes)",
                                        "VALUES",
                                        "($1, $2, $3, $4, $5, $6)")))

insert_table <- dplyr::bind_rows(insert_table,
                          c(table = "category",
                            sql = paste("INSERT INTO category",
                                        "(id, name, parent_id, source_id)",
                                        "VALUES",
                                        "($1, $2, $3, $4)")))


purrr::map2(insert_table$table, insert_table$sql, ~
              write_multiple_rows(master_list_surs, con, .x, .y, full))

