
df <-
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

# Load necessary library
library(dplyr)
# Load necessary library
library(dplyr)

# Assuming your data frame is named df with columns: locality, caterpillar, and parasitoid

# Count the number of caterpillars in each locality
abundance <- df %>%
  group_by(locality) %>%
  summarise(count = n(), .groups = 'drop')

# Randomly resample the data for each locality based on the abundance number
set.seed(123)  # Setting seed for reproducibility
resampled_df <- df %>%
  group_by(locality) %>%
  group_modify(~ .x[sample(1:nrow(.x), size = min(nrow(.x), abundance$count[abundance$locality == unique(.x$locality)])), ])

# Count interactions between caterpillar and parasitoid in the resampled data
interaction_count <- resampled_df %>%
  group_by(locality, CAT_sp, PAR_sp) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate the number of localities where each interaction is detected and not detected
interaction_presence <- interaction_count %>%
  group_by(CAT_sp, PAR_sp) %>%
  summarise(
    localities_detected = n(),
    localities_not_detected = length(unique(df$locality)) - n(),
    .groups = 'drop'
  )

# Display the final result
print(interaction_presence)




#. IGNORE THIS
# Count interactions between caterpillar and parasitoid in the original data
interaction_count <- df %>%
  group_by(locality, CAT_sp, PAR_sp) %>%
  summarise(count = n(), .groups = 'drop')

# Add interaction type column
interaction_count <- interaction_count %>%
  mutate(interaction_type = ifelse(is.na(PAR_sp), "Without Parasitoid", "With Parasitoid"))

# Summarize interaction types by locality, caterpillar species, and interaction type
interaction_presence <- interaction_count %>%
  group_by(locality, CAT_sp, interaction_type) %>%
  summarise(
    count = sum(count),
    .groups = 'drop'
  )

# Display the final result
print(interaction_presence)
