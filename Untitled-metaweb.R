#----------------------------------------------------------#
#
#
#                   Cascading diversity
#
#                Bray-Curtis dissimilarity
#
#
#                 M. Libra,  O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Estimate the Bray-Curtis dissimilarity between two localities for
#   parasitoids and catepillars.


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

# Load necessary libraries
library(tidyverse)
library(here)
library(vegan)


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


#----------------------------------------------------------#
# 2. Helper functions -----
#----------------------------------------------------------#

get_speciec_counts <- function(data_source) {
  data_source %>%
    dplyr::group_by(locality, sp) %>%
    dplyr::count() %>%
    tidyr::pivot_wider(
      names_from = locality,
      values_from = n,
      values_fill = 0
    ) %>%
    as.data.frame() %>%
    return()
}

get_species_matrix <- function(data_source) {
  data_source %>%
    tibble::column_to_rownames(var = "sp") %>%
    t() %>%
    as.matrix()
}

get_bray_curtis <- function(data_source) {
  bray_curtis <-
    data_source %>%
    vegan::vegdist(method = "bray") %>%
    as.matrix()
  
  res <-
    bray_curtis[1, 2]
  
  return(res)
}

get_smaller_locality_size <- function(data_source) {
  data_source %>%
    dplyr::group_by(locality) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::summarize(min(count)) %>%
    dplyr::pull()
}

get_locality_name_by_size <- function(data_source, type = c("min", "max")) {
  type <- match.arg(type)
  data_summed <-
    data_source %>%
    dplyr::group_by(locality) %>%
    dplyr::summarize(count = n())
  
  if (
    type == "max"
  ) {
    res <-
      dplyr::filter(data_summed, count == max(count))
  } else {
    res <-
      dplyr::filter(data_summed, count == min(count))
  }
  
  res %>%
    dplyr::pull(locality) %>%
    return()
}

resample_and_calculate_bray_curtis <- function(
    data_source,
    small_locality_size,
    bigger_locality_name,
    small_locality_name,
    resample = TRUE) {
  subsampled_data <-
    data_source %>%
    dplyr::filter(locality == bigger_locality_name) %>%
    dplyr::sample_n(small_locality_size, replace = resample)
  
  combined_data <-
    dplyr::bind_rows(
      subsampled_data,
      data_source %>%
        dplyr::filter(locality == small_locality_name)
    )
  
  species_counts <-
    get_speciec_counts(combined_data)
  
  species_matrix <-
    get_species_matrix(species_counts)
  
  res <-
    get_bray_curtis(species_matrix)
  
  return(res)
}

#----------------------------------------------------------#
# 3. Parasitoids -----
#----------------------------------------------------------#

data_parasitoids <-
  data_master %>%
  dplyr::filter(
    guild == "PAR",
    Par_remove == "0",
    locality %in% c("Ohu1", "Ohu2")
  ) %>%
  dplyr::select(
    locality,
    sp = PAR_sp
  )

data_parasitoids_sp_count <-
  get_speciec_counts(data_parasitoids)

# View the species counts table
print(head(data_parasitoids_sp_count))

# Prepare data for Bray-Curtis dissimilarity
data_parasitoids_sp_count_matrix <-
  get_species_matrix(data_parasitoids_sp_count)

# Calculate Bray-Curtis dissimilarity
parasitoids_bray_curtis <-
  get_bray_curtis(data_parasitoids_sp_count_matrix)

# View the Bray-Curtis dissimilarity matrix
print(parasitoids_bray_curtis) # 0.4143921

set.seed(9999) # Set seed for reproducibility
parasitoids_bray_curtis_resampled <-
  purrr::map_dbl(
    .progress = TRUE,
    .x = 1:999,
    .f = ~ resample_and_calculate_bray_curtis(
      data_source = data_parasitoids,
      small_locality_size = get_smaller_locality_size(data_parasitoids),
      bigger_locality_name = get_locality_name_by_size(data_parasitoids, "max"),
      small_locality_name = get_locality_name_by_size(data_parasitoids, "min")
    )
  )

# View the results
print(summary(parasitoids_bray_curtis_resampled))

parasitoids_bray_curtis_resampled_mean <-
  mean(parasitoids_bray_curtis_resampled)
parasitoids_bray_curtis_resampled_sd <-
  sd(parasitoids_bray_curtis_resampled) # 0.01525228

# Print the mean Bray-Curtis dissimilarity
print(parasitoids_bray_curtis_resampled_mean) # 0.379075


#----------------------------------------------------------#
# 4. Catepillars -----
#----------------------------------------------------------#

data_catepillars <-
  data_master %>%
  dplyr::filter(
    guild == "CAT",
    remove_cat == "0",
    locality %in% c("Ohu1", "Ohu2")
  ) %>%
  dplyr::select(
    locality,
    sp = CAT_sp
  )

data_catepillars_sp_count <-
  get_speciec_counts(data_catepillars)

# View the species counts table
print(head(data_catepillars_sp_count))

# Prepare data for Bray-Curtis dissimilarity
data_catepillars_sp_count_matrix <-
  get_species_matrix(data_catepillars_sp_count)

# Calculate Bray-Curtis dissimilarity
catepillars_bray_curtis <-
  get_bray_curtis(data_catepillars_sp_count_matrix)

# View the Bray-Curtis dissimilarity matrix
print(catepillars_bray_curtis) # 0.3057603


set.seed(9999) # Set seed for reproducibility
catepillars_bray_curtis_resampled <-
  purrr::map_dbl(
    .progress = TRUE,
    .x = 1:999,
    .f = ~ resample_and_calculate_bray_curtis(
      data_source = data_catepillars,
      small_locality_size = get_smaller_locality_size(data_catepillars),
      bigger_locality_name = get_locality_name_by_size(data_catepillars, "max"),
      small_locality_name = get_locality_name_by_size(data_catepillars, "min")
    )
  )

# View the results
print(summary(catepillars_bray_curtis_resampled))

catepillars_bray_curtis_resampled_mean <-
  mean(catepillars_bray_curtis_resampled)
catepillars_bray_curtis_resampled_sd <-
  sd(catepillars_bray_curtis_resampled)

# Print the mean Bray-Curtis dissimilarity
print(catepillars_bray_curtis_resampled_mean)