#' sample elements from subgroup

#' @param data tibble
#' @param size the number  subgroups you want  sample
#' @param ... the column names  the subgroup you want  sample

#' @importFrom dplyr group_by quos cur_group_id filter mutate pull
#' @importFrom magrittr %>%
#' @description Taken and adapted from https://www.tjmahr.com/sample-n-groups/
#' @return tibble with elements belonging  the sampled subgroups
#' @export

#' @examples library(dplyr)
#'  mtcars %>% sample_n_of(2,cyl) %>% distinct(cyl)
#'
#' mtcars %>%
#' mutate(rowid = 1:n()) %>%
#' group_by(cyl) %>%
#' do(sample_n_of(., 2,rowid))

sample_n_of <- function(data, size,  ...) {
dots <- quos(...)

group_ids <- data %>%
    group_by(!!! dots, .drop = FALSE) %>%
    mutate(grp_id = cur_group_id()) %>%
    pull(grp_id)

if(length(unique(group_ids))< size){
  warning(paste0("Argument size large than the population ", dots, ". Setting size to the size of the population"))
  size <- length(unique(group_ids))
  }
sampled_groups <- sample(unique(group_ids), size)

data %>%
    filter(group_ids %in% sampled_groups)
}
