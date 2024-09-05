library(dplyr)
library(tidyr)


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
# Count the number of caterpillars for each locality
caterpillar_counts_per_locality <- data %>%
  group_by(locality) %>%
  summarise(caterpillar_count = n(), .groups = 'drop')

# Function to randomly sample records from each locality according to the caterpillar count
sample_records <- function(df, n) {
  df %>% sample_n(n)
}

# Randomly sample the same number of records from each locality
set.seed(123) # For reproducibility
sampled_data <- data %>%
  group_by(locality) %>%
  group_modify(~ sample_n(.x, min(nrow(.x), caterpillar_counts_per_locality$caterpillar_count[match(.x$locality[1], caterpillar_counts_per_locality$locality)]))) %>%
  ungroup()

# Count the total number of sites where each caterpillar species is detected
caterpillar_site_counts <- sampled_data %>%
  group_by(CAT_sp) %>%
  summarise(total_caterpillar_sites = n_distinct(locality), .groups = 'drop')

# Count the total number of sites where each caterpillar-parasitoid interaction exists
interaction_site_counts <- sampled_data %>%
  group_by(CAT_sp, PAR_sp) %>%
  summarise(total_interaction_sites = n_distinct(locality), .groups = 'drop')

# Calculate the total abundance of parasitoids for each caterpillar species
parasitoid_abundance <- sampled_data %>%
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







Explanation:
  Count caterpillar detection sites: We count the total number of distinct sites where each caterpillar species is detected.
Count interaction sites: We count the total number of distinct sites where each caterpillar-parasitoid interaction occurs.
Calculate parasitoid abundance: We calculate the total abundance of parasitoids for each caterpillar species.
Merge interaction site counts and parasitoid abundance: We combine the counts of interaction sites and parasitoid abundance.
Merge caterpillar site counts with interaction counts: We combine the counts of caterpillar detection sites with the interaction counts.
Add rows for caterpillars without parasitoids: We add rows for caterpillars that do not have parasitoids.
Combine results: We combine all the results and sort the final data frame.
Display the final result: Print the resulting data frame.