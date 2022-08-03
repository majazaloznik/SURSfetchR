# this is a setup file that is supposed to be run before all the tests.

rsp <- get_API_response()
out <- parse_structAPI_response(rsp)
full <- get_full_structure(out)
mat_h <- get_matrix_hierarchy(full)



api_list <- pxweb::pxweb_get("https://pxweb.stat.si/SiStatData/api/v1/sl/Data")
df <- as.data.frame(api_list)[1:5,]
matrixez_w_mtdt <- fill_listcolumn_w_mtdt(df)
