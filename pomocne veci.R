

# Calculate the average number of host species per parasitoid species
library(dplyr) # Load the dplyr package for data manipulation


testik_hs <-
  main_table %>%
  dplyr::filter(Par_remove == "0", remove_cat == "0", singlepara_1 == "0") %>% # remove unidentified species of parasitoids and caterpillars
  dplyr::select(locality, PAR_sp, CAT_sp, PLANT_sp,  )

average_hosts_per_parasitoid <- testik_hs %>%
  group_by(PAR_sp) %>%
  summarise(avg_hosts = mean(n_distinct(CAT_sp)))

# Plot a histogram of the averages
hist(average_hosts_per_parasitoid$avg_hosts, 
     main = "Average Number of Host Species per Parasitoid Species",
     xlab = "Average Number of Host Species",
     ylab = "Frequency",
     col = "skyblue",
     border = "black",
     breaks = 15)


testik_hs_cat <-
  main_table %>%
  dplyr::filter( remove_cat == "0", singlecat == "0") %>% # remove unidentified species of parasitoids and caterpillars
  dplyr::select(locality, PAR_sp, CAT_sp, PLANT_sp,  )

average_hosts_per_caterpillar <- testik_hs_cat %>%
  group_by(CAT_sp) %>%
  summarise(avg_hosts = mean(n_distinct(PLANT_sp)))

# Plot a histogram of the averages
hist(average_hosts_per_caterpillar$avg_hosts, 
     main = "Average Number of Host Species per Parasitoid Species",
     xlab = "Average Number of Host Species",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")


ggplot(average_hosts_per_caterpillar, aes(x = avg_hosts)) + 
  geom_histogram()






#reOhu1 <- subsampl_cat %>%
  filter(locality == "Ohu1")


#cats: Ohu1 - 4873 Ohu2- 3255
# par Ohu1 - 
Ohu3_cat <- sample_n(reOhu1, 3255)




# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Define the function
subsample_data <- function(dataset, sample_size = 3255, iterations = 20) {
  # Check if dataset has at least sample_size records
  if (nrow(dataset) < sample_size) {
    stop("The dataset has fewer records than the sample size.")
  }
  
  # Create an empty list to store the results
  subsampled_datasets <- vector("list", iterations)
  
  # Perform subsampling for the specified number of iterations
  for (i in 1:iterations) {
    subsampled_datasets[[i]] <- dataset %>% sample_n(sample_size)
  }
  
  return(subsampled_datasets)
}

# Example usage
# Assuming 'datasetA' is your dataset with 300 records
# datasetA <- read.csv("your_dataset.csv")

# Run the function
results <- subsample_data(reOhu1, sample_size = 3255, iterations = 20)

# Save each subsampled dataset to a file (optional)
for (i in 1:length(results)) {
  write.csv(results[[i]], paste0("subsampled_dataset_", i, ".csv"), row.names = FALSE)
}

# Check the first subsampled dataset
print(results[[1]])


##########################################
library(dplyr)
datasetA <- main_table %>%
  dplyr::filter(remove_cat == "0", locality == "Ohu1") %>% # remove unidentified species of parasitoids and caterpillars
  dplyr::select(CAT_sp)
# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Define the function
subsample_and_aggregate <- function(dataset, sample_size = 150, iterations = 20, host_plant_col = "host_plant") {
  # Check if dataset has at least sample_size records
  if (nrow(dataset) < sample_size) {
    stop("The dataset has fewer records than the sample size.")
  }
  
  # Create an empty list to store the results
  subsampled_aggregates <- vector("list", iterations)
  
  # Perform subsampling and aggregation for the specified number of iterations
  for (i in 1:iterations) {
    # Subsample the dataset
    subsample <- dataset %>% sample_n(sample_size)
    
    # Aggregate the total abundance for each host plant species
    aggregate <- subsample %>%
      group_by(!!sym(CAT_sp)) %>%
      summarise(total_abundance = n()) %>%
      arrange(!!sym(CAT_sp))
    
    # Rename the total abundance column to indicate the iteration number
    colnames(aggregate)[2] <- paste0("iteration_", i)
    
    # Store the aggregate table in the list
    subsampled_aggregates[[i]] <- aggregate
  }
  
  # Merge all the aggregated tables into one
  merged_result <- Reduce(function(x, y) full_join(x, y, by = PLANT_sp), subsampled_aggregates)
  
  # Replace NAs with zeros (for species not present in some iterations)
  merged_result[is.na(merged_result)] <- 0
  
  return(merged_result)
}

# Example usage
# Assuming 'datasetA' is your dataset with 300 records and 'host_plant' is the column with host plant species information
# datasetA <- read.csv("your_dataset.csv")

# Run the function
results <- subsample_and_aggregate(datasetA, sample_size = 150, iterations = 20, host_plant_col = "host_plant")

# Check the merged result
print(results)

