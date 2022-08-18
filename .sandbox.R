devtools::install_github("majazaloznik/SURSfetchR", dependencies = FALSE)
library(SURSfetchR)
library(tidyr)
library(dplyr)
library(purrr)

operating_hierarchy <- readRDS("../../analysis/mesecni_kazalniki/data/operating_hierarchy.rds")
full_hierarchy <- readRDS("../../analysis/mesecni_kazalniki/data/full_hierarchy.rds")

get_level_combos2 <- function(x, y) {
  x %>%
    dplyr::filter(dimension_name %in% unlist(y)) %>%
    pull(levels) %>%
    purrr::map( "valueTexts") %>%
    expand.grid()
}

operating_hierarchy <- operating_hierarchy %>%
  rowwise() %>%
  mutate(level_combos = list(get_level_combos2(levelz, dim_names_notime)))
saveRDS(operating_hierarchy, "../../analysis/mesecni_kazalniki/data/operating_hierarchy_full.rds")

x <- operating_hierarchy[1:5,]

x %>%
 unnest(level_combos) -> y

operating_hierarchy %>%
  filter(no_points == no_series) -> xx
which.max(operating_hierarchy$no_points)
full_hierarchy %>%
  filter(arch == TRUE) %>%
  summarise(data_points = sum(no_points))
