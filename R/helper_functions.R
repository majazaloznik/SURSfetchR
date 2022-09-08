#' Expanding from levels to series codes and titles
#'
#' These two helper functions take a set of non-time levels for a single table
#' and expand the grid to get all of their combinations and then either return
#' a dataframe with columns for each level code, or one where the level texts
#' have been concatenated into the series titles.
#' @param code_nopx. code e.g. 0300230S
#' @param unit_id num text of unit id
#' @return dataframe with expanded levels, one column per non-time dimension plus
#' unit_id for the level codes and sinle column with series titles for the other one.
#' @rdname expanding
#' @keywords internal
expand_to_level_codes <- function (code_no, unit_id) {
  get_table_levels(code_no) %>%
    dplyr::filter(!time) %>%
    dplyr::pull(levels) %>%
    purrr::map("values") %>%
    expand.grid(stringsAsFactors = FALSE) %>%
    dplyr::mutate(unit_id = unit_id)
}
#' @rdname expanding
#' @keywords internal
expand_to_series_titles <- function(code_no){
  get_table_levels(code_no) %>%
    dplyr::filter(!time) %>%
    dplyr::pull(levels) %>%
    purrr::map("valueTexts") %>%
    expand.grid() %>%
    tidyr::unite("series_title", dplyr::everything(), sep = " - ")
}

#' Joining the unit tables from either meritve or valuenotes.
#'
#' These two helper functions take the expanded level code tables and
#' join the appropriate unit_ids from either the meritve or the valuenotes
#' tables.
#'
#' @param expanded_level_codes dataframe output of \link[SURSfetchR]{expand_to_level_codes}
#' @param meritve_dim_no numeric output of \link[SURSfetchR]{get_meritve_no}
#' @param units_by_meritve_levels dataframe output of \link[SURSfetchR]{get_unit_levels_from_meritve}
#' @param valuenotes_dim_no numeric output of \link[SURSfetchR]{get_valuenotes_no}
#' @param units_by_levels dataframe output of \link[SURSfetchR]{get_unit_levels_from_meritve}
#' @return datafram from expanded_level_codes with added `unit_id` column
#'
#' @rdname add_units
#' @keywords internal
add_meritve_level_units <- function(expanded_level_codes, meritve_dim_no,
                                    units_by_meritve_levels){
  expanded_level_codes %>%
    dplyr::select(-unit_id) %>%
    dplyr::rename("level_value" := !!(paste0("Var", meritve_dim_no))) %>%
    dplyr::left_join(units_by_meritve_levels, by = c("level_value" = "level_value")) %>%
    dplyr::rename(!!(paste0("Var", meritve_dim_no)) := "level_value") %>%
    dplyr::select(-tab_dim_id, -unit)
}

#' @rdname add_units
#' @keywords internal
add_valuenotes_level_units <- function(expanded_level_codes, valuenotes_dim_no,
                                       units_by_levels){
  expanded_level_codes %>%
    dplyr::select(-unit_id) %>%
    dplyr::rename("level_value" := !!(paste0("Var", valuenotes_dim_no))) %>%
    dplyr::left_join(units_by_levels, by = c("level_value" = "level_value")) %>%
    dplyr::rename(!!(paste0("Var", valuenotes_dim_no)) := "level_value") %>%
    dplyr::select(-dim_name, -level_text, -tab_dim_id)
}
