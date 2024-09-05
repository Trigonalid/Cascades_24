#----------------------------------------------------------#
#
#
#                   Cascading diversity
#
#                   Subsampling foodwebs
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
library(bipartite)
library(ggpubr)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_bigger <-
  readr::read_delim(
    here::here("DATA/bigger_dataset.csv"),
    delim = ";"
  ) %>%
  rlang::set_names(c("locality", "caterpillar", "plant"))

data_smaller <-
  readr::read_delim(
    here::here("DATA/smaller_dataset.csv"),
    delim = ";"
  ) %>%
  rlang::set_names(c("locality", "caterpillar", "parasitoid"))

#----------------------------------------------------------#
# 2. Data wrangling -----
#----------------------------------------------------------#

# Check for NA or empty column names
if (any(is.na(names(data_bigger))) | any(names(data_bigger) == "") |
  any(is.na(names(data_smaller))) | any(names(data_smaller) == "")) {
  stop("Data frames contain NA or empty column names.")
}

# Summarize counts by locality
count_smaller <-
  data_smaller %>%
  dplyr::group_by(locality) %>%
  dplyr::summarise(count = n())

count_bigger <-
  data_bigger %>%
  dplyr::group_by(locality) %>%
  dplyr::summarise(count = n())

#----------------------------------------------------------#
# 3. Subsampling -----
#----------------------------------------------------------#

# - 2.1 helper function -----

# Function to create a matrix for each locality
create_matrix <- function(data_locality, vec_plants, vec_catepillars) {
  matrix_local_data <-
    data_locality %>%
    dplyr::count(plant, caterpillar) %>%
    tidyr::pivot_wider(
      names_from = caterpillar,
      values_from = n,
      values_fill = list(n = 0)
    ) %>%
    tibble::column_to_rownames("plant") %>%
    as.matrix()

  # cretae dummy matrix with  all plants and caterpillars, filling with zeros
  matrix_full_data <-
    matrix(0,
      nrow = length(vec_plants), ncol = length(vec_catepillars),
      dimnames = list(vec_plants, vec_catepillars)
    )

  common_plants <-
    intersect(unique(data_locality$plant), vec_plants)

  common_caterpillars <-
    intersect(unique(data_locality$caterpillar), vec_catepillars)

  # fill the matrix with the data
  matrix_full_data[common_plants, common_caterpillars] <-
    matrix_local_data[common_plants, common_caterpillars]

  return(matrix_full_data)
}

# Function to compare matrices using betalinkr
compare_matrices <- function(matrix1, matrix2) {
  webs <-
    array(
      dim = c(dim(matrix1), 2)
    )

  webs[, , 1] <- matrix1
  webs[, , 2] <- matrix2

  result <-
    bipartite::betalinkr_multi(webs)
  return(result)
}

# - 2.2 subsampling -----
set.seed(9999) # Set seed for reproducibility

# iterate over the data to subsample
all_comparison_results <-
  c(1:100) %>%
  rlang::set_names() %>%
  purrr::map(
    .progress = TRUE,
    .f = ~ {
      data_subsampled <-
        data_bigger %>%
        dplyr::group_by(locality) %>%
        dplyr::group_modify(
          .f = ~ {
            sel_locality_name <-
              .y$locality

            sel_locality_count <-
              count_smaller$count[count_smaller$locality == sel_locality_name]

            if (
              length(sel_locality_count) == 0
            ) {
              sel_locality_count <- 0
            }

            .x %>%
              dplyr::sample_n(size = sel_locality_count, replace = TRUE)
          }
        ) %>%
        ungroup()

      # Get all unique plants and caterpillars across all localities
      vector_all_plants <-
        unique(data_subsampled$plant)
      vector_all_caterpillars <-
        unique(data_subsampled$caterpillar)

      vector_localities <-
        unique(data_subsampled$locality) %>%
        rlang::set_names()

      # Create matrix for each locality
      locality_matrices <-
        vector_localities %>%
        purrr::map(
          .f = ~ data_subsampled %>%
            dplyr::filter(locality == .x) %>%
            create_matrix(
              data_locality = .,
              vec_plants = vector_all_plants,
              vec_catepillars = vector_all_caterpillars
            )
        )

      # Compare matrices for each locality
      comparison_results <-
        utils::combn(
          vector_localities,
          m = 2
        ) %>%
        t() %>%
        as.data.frame() %>%
        rlang::set_names(
          c("locality1", "locality2")
        ) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          comparison = purrr::map2(
            .x = locality1,
            .y = locality2,
            .f = ~ compare_matrices(
              locality_matrices[[.x]],
              locality_matrices[[.y]]
            )
          )
        ) %>%
        tidyr::unnest_wider(comparison)

      return(comparison_results)
    }
  ) %>%
  dplyr::bind_rows(
    .id = "iteration"
  ) %>%
  dplyr::mutate(iteration = as.numeric(iteration))

summary(all_comparison_results)


#----------------------------------------------------------#
# 4. Visualisations -----
#----------------------------------------------------------#


# merge with the original data
# Parasitoid-Caterpillar
FW_full_PC_figure <- bind_rows(FW_OS_full_PC_2, FW_WN_full_PC_2, FW_ST_full_PC_2, FW_S_full_PC_2)

FW_full_PC_figure <- FW_full_PC_figure %>%
  select(Locality_A, hodnota, guild) %>%
  mutate(dataset_type = "PAR-CAT")

colnames(FW_full_PC_figure) <- c("RowNames", "value", "Suffix", "dataset_type")
head(FW_full_PC_figure)

# Caterpillar-Plants
FW_full_CPL_figure <- bind_rows(FW_OS_full_CPL_2, FW_S_full_CPL_2, FW_ST_full_CPL_2, FW_WN_full_CPL_2)
FW_full_CPL_figure <- FW_full_CPL_figure %>%
  select(Locality_A, hodnota, guild) %>%
  mutate(dataset_type = "CAT-PLANT")
colnames(FW_full_CPL_figure) <- c("RowNames", "value", "Suffix", "dataset_type")
head(FW_full_CPL_figure)



head(data_subsample)


merged_df <- rbind(FW_full_PC_figure, data_subsample, FW_full_CPL_figure)
head(merged_df)

# Stats

graf_S <- merged_df %>%
  filter(Suffix == "S")
graf_S$dataset_type <- factor(graf_S$dataset_type, levels = c("PAR-CAT", "CAT-PLANT", "Subsampled"))

graf_ST <- merged_df %>%
  filter(Suffix == "ST")
graf_ST$dataset_type <- factor(graf_ST$dataset_type, levels = c("PAR-CAT", "CAT-PLANT", "Subsampled"))

graf_WN <- merged_df %>%
  filter(Suffix == "WN")
graf_WN$dataset_type <- factor(graf_WN$dataset_type, levels = c("PAR-CAT", "CAT-PLANT", "Subsampled"))

graf_OS <- merged_df %>%
  filter(Suffix == "OS")
graf_OS$dataset_type <- factor(graf_OS$dataset_type, levels = c("PAR-CAT", "CAT-PLANT", "Subsampled"))
# test of the significant difference
graf_S_aov <- graf_S %>%
  filter(dataset_type %in% c("Subsampled", "CAT-PLANT"))
S_aoe <- aov(value ~ dataset_type, data = graf_S_aov)
summary(S_aoe)

graf_ST_aov <- graf_ST %>%
  filter(dataset_type %in% c("Subsampled", "CAT-PLANT"))
ST_aoe <- aov(value ~ dataset_type, data = graf_ST_aov)
summary(ST_aoe)

graf_OS_aov <- graf_OS %>%
  filter(dataset_type %in% c("Subsampled", "CAT-PLANT"))
OS_aoe <- aov(value ~ dataset_type, data = graf_OS_aov)
summary(OS_aoe)

graf_WN_aov <- graf_WN %>%
  filter(dataset_type %in% c("Subsampled", "CAT-PLANT"))
WN_aoe <- aov(value ~ dataset_type, data = graf_WN_aov)
summary(WN_aoe)

# Figures
Fig_S <- ggplot(graf_S, aes(x = dataset_type, y = value)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  geom_signif(
    comparisons = list(c("PAR-CAT","CAT-PLANT"), 
                       c("CAT-PLANT","Subsampled")),
  map_signif_level = TRUE) +
  labs(title = "S", x = "", y = " Dissimilarity in species composition") +
  theme_classic()


Fig_ST <- ggplot(graf_ST, aes(x = dataset_type, y = value)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  geom_signif(
    comparisons = list(c("PAR-CAT","CAT-PLANT"), 
                       c("CAT-PLANT","Subsampled")),
    map_signif_level = TRUE) +
  labs(title = "ST", x = "", y = "Dissimilarity  by species turnover") +
  theme_classic()

Fig_OS <- ggplot(graf_OS, aes(x = dataset_type, y = value)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  geom_signif(
    comparisons = list(c("PAR-CAT","CAT-PLANT"), 
                       c("CAT-PLANT","Subsampled")),
    map_signif_level = TRUE) +
  labs(title = "OS", x = "", y = "Dissimilairty explained by rewiring") +
  theme_classic()

Fig_WN <- ggplot(graf_WN, aes(x = dataset_type, y = value)) +
  geom_violin() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  geom_signif(
    comparisons = list(c("PAR-CAT","CAT-PLANT"), 
                       c("CAT-PLANT","Subsampled")),
    map_signif_level = TRUE) +
  labs(title = "WN", x = "", y = "General dissimilairty between the two networks") +
  theme_classic()

fig_subsampling <- ggarrange(Fig_WN, Fig_OS, Fig_ST, common.legend = TRUE, legend = "bottom", nrow = 1)


fig_subsampling
ggsave("output/fig/fig_subsampling.png",
       fig_subsampling,
  height = 9,
  width = 22,
  units = "cm"
)

#
