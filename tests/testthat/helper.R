library(dittodb)
# # this is a setup file that is supposed to be run before all the tests.
# # have now moved the object to the testdata folder cuz it was taking too long
# rsp <- get_API_response()
# out <- parse_structAPI_response(rsp)
# full <- get_full_structure(out)
# mat_h <- get_matrix_hierarchy(full)
#
# api_list <- pxweb::pxweb_get("https://pxweb.stat.si/SiStatData/api/v1/sl/Data")
# set.seed(42)
# df <- as.data.frame(api_list)[sample(1:3000,5),]
# tmp <- fill_listcolumn_w_levelz(df)
# matrixez_w_mtdt <- pull_levels(tmp)
#
# saveRDS(matrixez_w_mtdt, "tests/testthat/testdata/matrixez_w_mtdt.rds")
# saveRDS(mat_h, "tests/testthat/testdata/mat_h.rds")
# saveRDS(full, "tests/testthat/testdata/full_h.rds")

# insert_table <- data.frame(table = character(),
#                            sql = character())
# insert_table <- bind_rows(insert_table,
#                           c(table = "table",
#                             sql = paste("INSERT INTO \"table\"",
#                                         "(code, name, source_id, url, notes)",
#                                         "VALUES",
#                                         "($1, $2, $3, $4, $5)")))
#
# insert_table <- bind_rows(insert_table,
#                           c(table = "category",
#                             sql = paste("INSERT INTO category",
#                                         "(id, name, source_id)",
#                                         "VALUES",
#                                         "($1, $2, $3)")))
#
# insert_table <- bind_rows(insert_table,
#                           c(table = "category_relationship",
#                             sql = paste("INSERT INTO category_relationship",
#                                         "(category_id, parent_id, source_id)",
#                                         "VALUES",
#                                         "($1, $2, $3)")))
#
# insert_table <- bind_rows(insert_table,
#                           c(table = "category_table",
#                             sql = paste("INSERT INTO category_table",
#                                         "(table_id, category_id, source_id)",
#                                         "VALUES",
#                                         "($1, $2, $3)")))
#
# insert_table <- bind_rows(insert_table,
#                           c(table = "table_dimensions",
#                             sql = paste("INSERT INTO table_dimensions",
#                                         "(table_id, dimension, time)",
#                                         "VALUES",
#                                         "($1, $2, $3)")))
#
#
# insert_table <- bind_rows(insert_table,
#                           c(table = "dimension_levels",
#                             sql = paste("INSERT INTO dimension_levels",
#                                         "(tab_dim_id, level_value, level_text)",
#                                         "VALUES",
#                                         "($1, $2, $3)")))
# insert_table <- bind_rows(insert_table,
#                           c(table = "unit",
#                             sql =  paste("INSERT INTO unit",
#                                          "(name)",
#                                          "VALUES",
#                                          "($1)")))
#
# insert_table <- bind_rows(insert_table,
#                           c(table = "series",
#                             sql =  paste("INSERT INTO series",
#                                          "(table_id, name_long,  code, interval_id, unit_id)",
#                                          "VALUES",
#                                          "($1, $2, $3, $4, $5)")))
#
# insert_table <- bind_rows(insert_table,
#                           c(table = "series_levels",
#                             sql =  paste("INSERT INTO series_levels",
#                                          "(series_id, tab_dim_id,  level_value)",
#                                          "VALUES",
#                                          "($1, $2, $3)")))
# insert_table <- bind_rows(insert_table,
#                           c(table = "vintage",
#                             sql =  paste("INSERT INTO vintage",
#                                          "(series_id, published)",
#                                          "VALUES",
#                                          "($1, $2)")))
#
#
# saveRDS(insert_table, "tests/testthat/testdata/insert_table.rds")
# insert_table <- readRDS("tests/testthat/testdata/insert_table.rds")
