# This code was run once and is here for archival purposes.
#
source("tests/testthat/helper-connection.R")

# start_db_capturing()
# con <- make_test_connection()
# SURSfetchR:::get_meritve_no(20, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# level_text_from_meritve <- UMARaccessR::sql_get_levels_from_dimension_id(63, con, schema = "test_platform")
# SURSfetchR:::get_unit_levels_from_meritve(level_text_from_meritve, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# x <- get_px_metadata("1700104S")$valuenotes[[1]][1]
# out <- SURSfetchR:::get_valuenotes_unit(x, con, "test_platform")
# x <- SURSfetchR:::get_single_unit_from_px("0457101S", con, "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# out3 <- SURSfetchR:::expand_to_level_codes("0300220S") %>%
#   dplyr::mutate(unit_id = NA)
# x <- UMARaccessR::sql_get_levels_from_dimension_id(6, con, "test_platform")
# units <- SURSfetchR:::get_unit_levels_from_meritve(x, con, "test_platform")
# out4 <- SURSfetchR:::add_meritve_level_units(out3, 2, units)
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# out5 <- SURSfetchR:::expand_to_level_codes("1700104S") %>%
#   dplyr::mutate(unit_id = NA)
# units <- SURSfetchR:::get_valuenotes_from_px("1700104S", con, "test_platform")
# out6 <- SURSfetchR:::add_valuenotes_level_units(out5, 2, units)
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# full <- readRDS(test_path("testdata", "full_h.rds"))
# prepare_category_table_table("1700104S", full, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# full <- readRDS(test_path("testdata", "full_h.rds"))
# prepare_table_dimensions_table("1700104S", con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# prepare_dimension_levels_table("1700104S", con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# prepare_series_table("1700104S", con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# prepare_series_levels_table("1700104S", con, "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# prepare_vintage_table("1700104S", con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# x <- prepare_data_table("1700104S", con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# x <- SURSfetchR:::expand_to_level_codes(14, con, schema = "test_platform")
# SURSfetchR:::expand_to_level_codes(2, con, schema = "test_platform")
# SURSfetchR:::expand_to_level_codes(15, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# SURSfetchR:::expand_to_series_titles(14, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# SURS_import_structure("2711808S", con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# x <- prepare_vintage_table("2711808S", con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_test_connection()
# x <- prepare_data_table("1700104S", con, schema = "test_platform")
# prepare_data_table("H240S", con, "test_platform")
# stop_db_capturing()

# start_db_capturing()
# con <- make_test_connection()
# x <- prepare_surs_data_for_insert("H240S", con, "test_platform")
# stop_db_capturing()

start_db_capturing()
con <- make_test_connection()
df <- prepare_vintage_table("1817902S", con, schema = "test_platform")
UMARimportR::insert_new_vintage(con, df, "test_platform")
prep_data <- prepare_surs_data_for_insert("0300260S", con, schema = "test_platform")
result <- UMARimportR::insert_prepared_data_points(prep_data, con, schema = "test_platform")
stop_db_capturing()

start_db_capturing()
con <- make_test_connection()
x <- SURS_import_data_points("0714621S", con, schema = "test_platform")
stop_db_capturing()

