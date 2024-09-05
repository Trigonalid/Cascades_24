# Load necessary libraries
library(dplyr)
library(tidyr)
library(bipartite)
library(tibble)
library(ggplot2)

# Load the data
data_bigger <- read.csv("~/ownCloud/000_/000_R_stat/Cascading_diversity/DATA/bigger_dataset.csv", sep=";")
data_smaller <- read.csv("~/ownCloud/000_/000_R_stat/Cascading_diversity/DATA/smaller_dataset.csv", sep=";")

# Ensure column names are consistent
colnames(data_bigger) <- c("locality", "caterpillar", "plant")
colnames(data_smaller) <- c("locality", "caterpillar", "parasitoid")

# Check for NA or empty column names
if (any(is.na(names(data_bigger))) | any(names(data_bigger) == "") |
    any(is.na(names(data_smaller))) | any(names(data_smaller) == "")) {
  stop("Data frames contain NA or empty column names.")
}

# Summarize counts by locality
count_smaller <- data_smaller %>%
  group_by(locality) %>%
  summarise(count = n())

count_bigger <- data_bigger %>%
  group_by(locality) %>%
  summarise(count = n())

# Function to create a matrix for each locality
create_matrix <- function(locality_data, all_plants, all_caterpillars) {
  matrix_data <- locality_data %>%
    count(plant, caterpillar) %>%
    pivot_wider(names_from = caterpillar, values_from = n, values_fill = list(n = 0)) %>%
    column_to_rownames("plant") %>%
    as.matrix()
  
  # Ensure the matrix has all plants and caterpillars, filling with zeros where necessary
  full_matrix <- matrix(0, nrow = length(all_plants), ncol = length(all_caterpillars),
                        dimnames = list(all_plants, all_caterpillars))
  common_plants <- intersect(rownames(matrix_data), all_plants)
  common_caterpillars <- intersect(colnames(matrix_data), all_caterpillars)
  full_matrix[common_plants, common_caterpillars] <- matrix_data[common_plants, common_caterpillars]
  
  return(full_matrix)
}

# Function to compare matrices using betalinkr
compare_matrices <- function(matrix1, matrix2) {
  webs <- array(dim = c(dim(matrix1), 2))
  webs[,,1] <- matrix1
  webs[,,2] <- matrix2
  result <- bipartite::betalinkr_multi(webs)
  return(result)
}

# Initialize a list to store all comparison results
all_comparison_results <- list()

# Iterate through the desired number of iterations
for (iteration in 1:100)  {
  # For each locality, randomly subsample records from the bigger dataset
  subsampled_data <- data_bigger %>%
    group_by(locality) %>%
    group_modify(~ {
      locality_name <- .y$locality
      count <- count_smaller$count[count_smaller$locality == locality_name]
      if (length(count) == 0) {
        count <- 0
      }
      .x %>% sample_n(size = count, replace = TRUE)
    }) %>%
    ungroup()
  
  # Get all unique plants and caterpillars across all localities
  all_plants <- unique(subsampled_data$plant)
  all_caterpillars <- unique(subsampled_data$caterpillar)
  
  # List to store the matrices
  locality_matrices <- list()
  
  # Create matrix for each locality and save to list
  unique_localities <- unique(subsampled_data$locality)
  for (locality in unique_localities) {
    locality_data <- subsampled_data %>%
      filter(locality == !!locality)
    
    locality_matrices[[locality]] <- create_matrix(locality_data, all_plants, all_caterpillars)
  }
  
  # Compare matrices for each locality
  comparison_results <- list()
  for (i in 1:(length(locality_matrices) - 1)) {
    for (j in (i + 1):length(locality_matrices)) {
      locality1 <- names(locality_matrices)[i]
      locality2 <- names(locality_matrices)[j]
      
      comparison_result <- compare_matrices(locality_matrices[[locality1]], locality_matrices[[locality2]])
      
      # Print debug information for each comparison result
      print(paste("Iteration:", iteration, "Comparison:", locality1, "vs", locality2))
      print(comparison_result)
      
      comparison_results[[paste(locality1, locality2, sep = "_vs_")]] <- comparison_result
    }
  }
  
  # Store the comparison results of this iteration
  all_comparison_results[[iteration]] <- comparison_results
}



# Combine the comparison results from all iterations into a single data frame
combined_results <- do.call(rbind, lapply(all_comparison_results, function(iter_result) {
  # Convert the iteration's comparison results into a data frame
  iteration_df <- as.data.frame(iter_result)
  # Add an "Iteration" column to indicate the iteration number
  iteration_df$Iteration <- names(all_comparison_results)[match(iter_result, all_comparison_results)]
  return(iteration_df)
}))

# Print or save the combined results
print(combined_results)

combined_results

# Remove values with i.j.
filtered_df <- t(combined_results[, !grepl("\\.(i|j)$", names(combined_results))])

# Display the filtered data frame
print(filtered_df)

data_with_stats <-cbind(filtered_df, Mean = rowMeans(filtered_df), SD = apply(filtered_df, 1, sd))
# Extract the Mean column
first_column <- as_tibble(data_with_stats[, "Mean"])
# Extract row names
row_names <- rownames(data_with_stats)

# Combine row names and first column into a data frame
result_rr <- data.frame(RowNames = row_names, FirstColumn = first_column)

# Print the result
print(result_rr) 

# Adding 
data_subsample <- result_rr %>%
  mutate(dataset_type = "Subsampled")

# Extract the suffix
data_subsample$Suffix <- sub(".*\\.", "", data_subsample$RowNames)

#merge with the original data
# Parasitoid-Caterpillar
FW_full_PC_figure <- bind_rows (FW_OS_full_PC_2,FW_WN_full_PC_2, FW_ST_full_PC_2, FW_S_full_PC_2)

FW_full_PC_figure <- FW_full_PC_figure %>% 
  select(Locality_A, hodnota, guild) %>% 
  mutate(dataset_type = "PAR-CAT")

colnames (FW_full_PC_figure) <- c("RowNames", "value", "Suffix", "dataset_type")
head(FW_full_PC_figure)

# Caterpillar-Plants
FW_full_CPL_figure<- bind_rows (FW_OS_full_CPL_2,FW_S_full_CPL_2, FW_ST_full_CPL_2, FW_WN_full_CPL_2)
FW_full_CPL_figure <- FW_full_CPL_figure %>% 
  select(Locality_A, hodnota, guild) %>% 
  mutate(dataset_type = "CAT-PLANT")
colnames (FW_full_CPL_figure) <- c("RowNames", "value", "Suffix", "dataset_type")
head(FW_full_CPL_figure)



head(data_subsample)


merged_df <- rbind(FW_full_PC_figure, data_subsample, FW_full_CPL_figure)
head(merged_df)

# Stats 

graf_S<-  merged_df  %>%
  filter(Suffix =="S")
graf_S$dataset_type <- factor(graf_S$dataset_type , levels=c("PAR-CAT", "CAT-PLANT", "Subsampled"))

graf_ST<-  merged_df  %>%
  filter(Suffix =="ST")
graf_ST$dataset_type <- factor(graf_ST$dataset_type , levels=c("PAR-CAT", "CAT-PLANT", "Subsampled"))

graf_WN<-  merged_df  %>%
  filter(Suffix =="WN")
graf_WN$dataset_type <- factor(graf_WN$dataset_type , levels=c("PAR-CAT", "CAT-PLANT", "Subsampled"))

graf_OS<-  merged_df  %>%
  filter(Suffix =="OS")
graf_OS$dataset_type <- factor(graf_OS$dataset_type , levels=c("PAR-CAT", "CAT-PLANT", "Subsampled"))
# test of the significant difference 
                   graf_S_aov <- graf_S %>% 
                     filter(dataset_type %in% c("Subsampled", "CAT-PLANT"))
                   S_aoe <- aov(value~dataset_type, data=graf_S_aov); summary(S_aoe)
                   
                   graf_ST_aov <- graf_ST %>% 
                     filter(dataset_type %in% c("Subsampled", "CAT-PLANT"))
                   ST_aoe <- aov(value~dataset_type, data=graf_ST_aov); summary(ST_aoe)
                   
                   graf_OS_aov <- graf_OS %>% 
                     filter(dataset_type %in% c("Subsampled", "CAT-PLANT"))
                   OS_aoe <- aov(value~dataset_type, data=graf_OS_aov); summary(OS_aoe)
                   
                   graf_WN_aov <- graf_WN %>% 
                     filter(dataset_type %in% c("Subsampled", "CAT-PLANT"))
                   WN_aoe <- aov(value~dataset_type, data=graf_WN_aov); summary(WN_aoe)
                
# Figures                    
                   Fig_S <- ggplot(graf_S, aes(x=dataset_type, y=value)) +
                     geom_boxplot() +
                     geom_jitter(color="black", size=0.4, alpha=0.9) +
                     labs(title="S",x="", y = " Dissimilarity in species composition") +
                     theme_classic()
                   
                   
                   Fig_ST <- ggplot(graf_ST, aes(x=dataset_type, y=value)) +
                     geom_boxplot() +
                     geom_jitter(color="black", size=0.4, alpha=0.9)+
                     labs(title="ST",x="", y = "The dissimilarity  by species turnover") +
                     theme_classic()
                   
                   Fig_OS <- ggplot(graf_OS, aes(x=dataset_type, y=value)) +
                     geom_boxplot() +
                     geom_jitter(color="black", size=0.4, alpha=0.9)+
                     labs(title="OS",x="", y = "Dissimilairty explained by rewiring") +
                     theme_classic()
                   
                   Fig_WN <- ggplot(graf_WN, aes(x=dataset_type, y=value)) +
                     geom_boxplot() +
                     geom_jitter(color="black", size=0.4, alpha=0.9)+
                     labs(title="WN",x="", y = "Dissimilairty between the two networks") +
                     theme_classic()
                   
                   fig_subsampling <-ggarrange(Fig_WN, Fig_S, Fig_OS, Fig_ST, common.legend = TRUE, legend = "bottom", nrow = 1)
                   
                   
                   fig_subsampling 
                   ggsave("output/fig/fig_subsampling.png",
                          fig_subsampling,
                          height = 12,
                          width = 16,
                          units = "cm")
                   
# 