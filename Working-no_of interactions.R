# Load necessary package

library(dplyr)



data <-
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

# Count the total number of sites where each caterpillar species is detected
caterpillar_site_counts <- data %>%
  group_by(CAT_sp) %>%
  summarise(total_caterpillar_sites = n_distinct(locality), .groups = 'drop')

# Count the total number of sites where each caterpillar-parasitoid interaction exists
interaction_site_counts <- data %>%
  group_by(CAT_sp, PAR_sp) %>%
  summarise(total_interaction_sites = n_distinct(locality), .groups = 'drop')

# Calculate the total abundance of parasitoids for each caterpillar species
parasitoid_abundance <- data %>%
  group_by(CAT_sp, PAR_sp) %>%
  summarise(parasitoid_abundance = n(), .groups = 'drop')

# Merge the interaction site counts with parasitoid abundance
interaction_counts <- interaction_site_counts %>%
  left_join(parasitoid_abundance, by = c("CAT_sp", "PAR_sp"))

# Merge the caterpillar site counts with the interaction counts
result <- caterpillar_site_counts %>%
  left_join(interaction_counts, by = "CAT_sp") %>%
  arrange(CAT_sp, PAR_sp)

# Add rows for caterpillars without parasitoids
no_parasitoid_rows <- caterpillar_site_counts %>%
  anti_join(interaction_counts, by = "CAT_sp") %>%
  mutate(PAR_sp = NA_character_, total_interaction_sites = 0, parasitoid_abundance = 0)

# Combine the results
final_result <- bind_rows(result, no_parasitoid_rows) %>%
  arrange(CAT_sp, PAR_sp)

# Display the final result
print(final_result)
