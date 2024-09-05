MASTER <- read.csv("~/ownCloud/000_/000_R_stat/Cascading_diversity/DATA/MASTER.csv", sep=";") %>% as_tibble() # loading the master file with all data available (detailed info with old sp_code etc check Master_2023 file)
Distance <- read.csv2("~/ownCloud/000_/000_R_stat/Cascading_diversity/DATA/Distance.csv", row.names=1) # loading distance matrix of localities
data_testing <- MASTER %>% 
  filter(remove_cat == "0") %>% 
  select(locality,CAT_sp, PAR_sp, guild, PLANT_sp)

parasitoid_data_2<-  MASTER  %>%
  filter(`guild` == "PAR", Par_remove == "0") %>% 
  select(locality, PAR_sp)
#### Original B-C
species_counts_orig_para <- parasitoid_data_2 %>%
  group_by(locality, PAR_sp) %>%
  tally() %>%
  spread(key = locality, value = n, fill = 0) %>%
  as.data.frame()

# Replace NA with 0 for species that do not appear in a locality
species_counts_orig_para[is.na(species_counts_orig_para)] <- 0

# View the species counts table
print(species_counts_orig_para)

# Prepare data for Bray-Curtis dissimilarity
species_matrix_orig_para <- t(species_counts_orig_para)
names(species_matrix_orig_para) <- as.matrix(species_matrix_orig_para[1, ])
species_matrix_orig_para <- species_matrix_orig_para[-1, ]
species_matrix_orig_para <-as.data.frame(apply(species_matrix_orig_para,2,  as.numeric))


# Calculate Bray-Curtis dissimilarity
bray_curtis_orig_para <- vegdist(species_matrix_orig_para, method = "bray")

# View the Bray-Curtis dissimilarity matrix
print(as.matrix(bray_curtis_orig_para)) # 0.4143921

# Assume your data frame is called 'parasitoid_data_2'
# Determine the size of the smaller locality
small_locality_size <- parasitoid_data_2 %>% 
  group_by(locality) %>% 
  summarize(count = n()) %>% 
  summarize(min(count)) %>% 
  pull()

# Identify the bigger locality
locality_sizes <- parasitoid_data_2 %>% 
  group_by(locality) %>% 
  summarize(count = n())

bigger_locality <- locality_sizes %>% 
  filter(count == max(count)) %>% 
  pull(locality)

# Randomly subsample from the bigger locality
set.seed(9999) # Set seed for reproducibility
subsampled_data <- parasitoid_data_2 %>%
  filter(locality == bigger_locality) %>%
  sample_n(small_locality_size)

# Combine subsampled data with the smaller locality data
small_locality <- locality_sizes %>% 
  filter(count == min(count)) %>% 
  pull(locality)

combined_data <- bind_rows(subsampled_data, parasitoid_data_2 %>% filter(locality == small_locality))

# Verify the size of combined data
combined_data %>% group_by(locality) %>% summarize(count = n())

# Create a table with species counts for each locality
species_counts <- combined_data %>%
  group_by(locality, PAR_sp) %>%
  tally() %>%
  spread(key = locality, value = n, fill = 0) %>%
  as.data.frame()

# Replace NA with 0 for species that do not appear in a locality
species_counts[is.na(species_counts)] <- 0

# View the species counts table
print(species_counts)

# Prepare data for Bray-Curtis dissimilarity
species_matrix <- t(species_counts)
names(species_matrix) <- as.matrix(species_matrix[1, ])
species_matrix <- species_matrix[-1, ]
species_matrix <-as.data.frame(apply(species_matrix,2,  as.numeric))


# Calculate Bray-Curtis dissimilarity
bray_curtis <- vegdist(species_matrix, method = "bray")

# View the Bray-Curtis dissimilarity matrix
print(as.matrix(bray_curtis))


# Function for repetition
resample_and_calculate <- function(data, small_locality_size, bigger_locality, small_locality) {
  set.seed(sample(1:10000, 1)) # Set a random seed for reproducibility
  subsampled_data <- data %>%
    filter(locality == bigger_locality) %>%
    sample_n(small_locality_size)
  
  combined_data <- bind_rows(subsampled_data, data %>% filter(locality == small_locality))
  
  species_counts <- combined_data %>%
    group_by(locality, PAR_sp) %>%
    tally() %>%
    spread(key = locality, value = n, fill = 0) %>%
    as.data.frame()
  
  species_matrix <- species_counts %>%
    column_to_rownames(var = "PAR_sp") %>%
    t() %>%
    as.matrix()
  
  bray_curtis <- vegdist(species_matrix, method = "bray")
  return(as.matrix(bray_curtis))
}


bray_curtis_results <- lapply(1:999, function(x) {
  resample_and_calculate(parasitoid_data_2, small_locality_size, bigger_locality, small_locality)
})

# View the results
print(bray_curtis_results)

bray_curtis_values <- sapply(bray_curtis_results, function(mat) mat[1, 2])
mean_bray_curtis <- mean(bray_curtis_values)
sd_bray_curtis_PAR <-sd(bray_curtis_values) #0.01525228

# Print the mean Bray-Curtis dissimilarity
print(mean_bray_curtis) #0.379075

