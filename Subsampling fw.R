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

filtered_df <- t(combined_results[, !grepl("\\.(i|j)$", names(combined_results))])

# Display the filtered data frame
print(filtered_df)

data_with_stats <- cbind(filtered_df, Mean = rowMeans(filtered_df), SD = apply(filtered_df, 1, sd))

# Print the result
print(data_with_stats)

dimnames <- dimnames(data_with_stats)

# Extract the suffixes
suffixes <- gsub(".*\\.", "", dimnames[[1]])

# Split the data based on the suffixes
split_data <- split(data_with_stats, suffixes)

write.csv(data_with_stats, "data_with_stats.csv")





FW_full_PC_figure<- bind_rows (FW_OS_full_PC_2,FW_WN_full_PC_2, FW_ST_full_PC_2, FW_ST.l_full_PC_2 , FW_ST.h_full_PC_2, FW_ST.lh_full_PC_2)
writexl::write_xlsx(FW_full_PC_results, "FW_full_PC_figure.xlsx")

FW_full_CPL_figure<- bind_rows (FW_OS_full_CPL_2,FW_S_full_CPL_2, FW_ST_full_CPL_2, FW_ST.l_full_CPL_2 , FW_ST.h_full_CPL_2, FW_ST.lh_full_CPL_2 )
writexl::write_xlsx(FW_full_CPL_figure, "FW_full_CPL_figure.xlsx")


# making a graph
data_with_stats_for_graf <- read_excel("data_with_stats_for_graf.xlsx")

data_with_stats_for_graf <- read_excel("data_with_stats_for_graf.xlsx", 
                                       +     col_types = c("numeric", "text"))

graf_S<-  data_with_stats_for_graf  %>%
  filter(type  %in% c("S-resampl", "S-PAR", "S-PLANT"))
graf_S<-as.numeric(graf_S$value

graf_ST<-  data_with_stats_for_graf  %>%
  filter(type  %in% c("ST-resampl", "ST-PAR", "ST-PLANT"))
graf_WN<-  data_with_stats_for_graf  %>%
  filter(type  %in% c("WN-resampl", "WN-PAR", "WN-PLANT"))
graf_OS<-  data_with_stats_for_graf  %>%
  filter(type  %in% c("OS-resampl", "OS-PAR", "OS-PLANT"))

# test of the significant difference 
graf_S_aov <- graf_S %>% 
  filter(type %in% c("S-resampl", "S-PLANT"))
S_aoe <- aov(value~type, data=graf_S_aov); summary(S_aoe)

graf_WN_aov <- graf_WN %>% 
  filter(type %in% c("WN-resampl", "WN-PLANT"))
WN_aoe <- aov(value~type, data=graf_WN_aov); summary(WN_aoe)

graf_OS_aov <- graf_OS %>% 
  filter(type %in% c("OS-resampl", "OS-PLANT"))
OS_aoe <- aov(value~type, data=graf_OS_aov); summary(OS_aoe)

graf_ST_aov <- graf_ST %>% 
  filter(type %in% c("ST-resampl", "ST-PLANT"))
ST_aoe <- aov(value~type, data=graf_ST_aov); summary(ST_aoe)



library(tidyverse)
library(hrbrthemes)
library(viridis)

Fig_S <- ggplot(graf_S, aes(x=type, y=value)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title="S",x="Dataset", y = "Dissimilairty of interactions") +
  theme_classic()


Fig_ST <- ggplot(graf_ST, aes(x=type, y=value)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title="ST",x="Dataset", y = "Dissimilairty of interactions") +
  theme_classic()

Fig_OS <- ggplot(graf_OS, aes(x=type, y=value)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title="OS",x="Dataset", y = "Dissimilairty of interactions") +
  theme_classic()

Fig_WN <- ggplot(graf_WN, aes(x=type, y=value)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title="WN",x="Dataset", y = "Dissimilairty of interactions") +
  theme_classic()

fig_subsampling <-ggarrange(Fig_WN, Fig_S, Fig_OS, Fig_ST, common.legend = TRUE, legend = "bottom", nrow = 1)


fig_subsampling 
ggsave("output/fig/fig_subsampling.png",
       Fig_3_B,
       height = 12,
       width = 16,
       units = "cm")


