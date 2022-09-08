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
