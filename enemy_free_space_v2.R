#----------------------------------------------------------#
#
#
#                   Cascading diversity
#
#                   Enemy-free space
#
#
#                 M. Libra,  O. Mottl
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

# Load necessary libraries
library(tidyverse)


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_master <-
  readr::read_delim(
    here::here(
      "DATA/MASTER.csv"
    ),
    delim = ";"
  ) %>%
  tibble::as_tibble()


data_work <-
  data_master %>%
  dplyr::filter(
    is.na(Par_remove) |
      Par_remove == "0"
  ) %>%
  dplyr::filter(remove_cat == "0") %>%
  dplyr::select(
    locality,
    plant,
    CAT_sp,
    PAR_sp
  )


count_n_of_no_parasiotids <- function(data_source) {
  data_source %>%
    dplyr::distinct(CAT_sp) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      n_localities_without_parasitoid = purrr::map_dbl(
        .progress = FALSE,
        .x = CAT_sp,
        .f = ~ data_source %>%
          dplyr::filter(CAT_sp == .x) %>%
          dplyr::distinct(locality, PAR_sp) %>%
          dplyr::group_by(locality) %>%
          tidyr::nest() %>%
          dplyr::mutate(
            has_only_na = purrr::map_lgl(
              .x = data,
              .f = ~ .x %>%
                dplyr::pull(PAR_sp) %>%
                is.na() %>%
                all()
            )
          ) %>%
          dplyr::pull(has_only_na) %>%
          sum()
      )
    )
}

res_observed <-
  count_n_of_no_parasiotids(data_work)

vec_localities <-
  unique(data_work$locality) %>%
  rlang::set_names()

set.seed(123) # Setting seed for reproducibility

res_int <-
  purrr::map_dbl(
    .progress = TRUE,
    .x = 1:25,
    .f = ~ {
      data_subsample <-
        data_work %>%
        dplyr::mutate(
          locality = sample(
            vec_localities,
            size = nrow(data_work),
            replace = TRUE
          )
        )
      res_subsambple <-
        count_n_of_no_parasiotids(data_subsample)

      res <-
        dplyr::full_join(
          res_observed,
          res_subsambple,
          by = "CAT_sp",
          suffix = c("_observed", "_subsampled")
        )

      res %>%
        dplyr::mutate(
          diff = n_localities_without_parasitoid_observed - n_localities_without_parasitoid_subsampled
        ) %>%
        dplyr::pull(diff) %>%
        mean() %>%
        return()
    }
  )
