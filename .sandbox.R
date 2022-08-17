devtools::install_github("majazaloznik/SURSfetchR", dependencies = FALSE)
library(SURSfetchR)
library(tidyr)
library(dplyr)
library(purrr)

operating_hierarchy <- readRDS("../../analysis/mesecni_kazalniki/data/operating_hierarchy.rds")

xx <- operating_hierarchy[1:5,]

get_level_combos2 <- function(x, y) {
  x %>%
    dplyr::filter(dimension_name %in% unlist(y)) %>%
    pull(levels) %>%
    purrr::map( "valueTexts") %>%
    expand.grid()
}
i <- 7
get_level_combos2(operating_hierarchy[i,]$levelz[[1]], operating_hierarchy[i,]$dim_names_notime)

operating_hierarchy %>%
  rowwise() %>%
  mutate(level_combos = list(get_level_combos2(levelz, dim_names_notime))) -> y

