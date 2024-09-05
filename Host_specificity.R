# Host-specificity of parastoids and caterpillars - all data included, WITH singletons and doubletons


# Load necessary packages
library(dplyr)
library(tidyverse)

# Sample dataset
data_master <-
  readr::read_delim(
    here::here(
      "DATA/MASTER.csv"
    ),
    delim = ";"
  ) %>%
  tibble::as_tibble()


#----------------------------------------------------------#
# 2. Data wrangling -----
#----------------------------------------------------------#

data<-
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

# Calculate host specificity for caterpillars
cat_specificity <- data %>%
  group_by(locality, CAT_sp) %>%
  summarize(unique_plants = n_distinct(plant)) %>%
  arrange(locality, CAT_sp)

# Calculate host specificity for parasitoids
par_specificity <- data %>%
  group_by(locality, PAR_sp) %>%
  summarize(unique_caterpillars = n_distinct(CAT_sp)) %>%
  arrange(locality, PAR_sp)

# Calculate average host specificity for caterpillars at each locality
avg_cat_specificity <- cat_specificity %>%
  group_by(locality) %>%
  summarize(mean_specificity = mean(unique_plants),
            sd_specificity = sd(unique_plants))

# Calculate average host specificity for parasitoids at each locality
avg_par_specificity <- par_specificity %>%
  group_by(locality) %>%
  summarize(mean_specificity = mean(unique_caterpillars),
            sd_specificity = sd(unique_caterpillars))

# Print the results
print(cat_specificity)
print(par_specificity)
print(avg_cat_specificity)
print(avg_par_specificity)


# Example results for caterpillar host specificity
cat_specificity

par_specificity

avg_par_specificity_loc <- avg_par_specificity %>%
  summarize(mean_specificity_par = mean(mean_specificity),
            sd_specificity_par = mean(sd_specificity))

avg_cat_specificity_loc <- avg_cat_specificity %>%
  summarize(mean_specificity_cat = mean(mean_specificity),
            sd_specificity_cat = mean(sd_specificity))


##############################################################################################################################
##############################################################################################################################

# Host-specificity of parastoids and caterpillars - all data included, WITHOUT singletons and doubletons


# Load necessary packages
library(dplyr)
library(tidyverse)

# Sample dataset
data_master <-
  readr::read_delim(
    here::here(
      "DATA/MASTER.csv"
    ),
    delim = ";"
  ) %>%
  tibble::as_tibble()


#----------------------------------------------------------#
# 2. Data wrangling -----
#----------------------------------------------------------#

data<-
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

# Calculate host specificity for caterpillars
cat_specificity <- data %>%
  group_by(locality, CAT_sp) %>%
  summarize(unique_plants = n_distinct(plant)) %>%
  arrange(locality, CAT_sp)

# Calculate host specificity for parasitoids
par_specificity <- data %>%
  group_by(locality, PAR_sp) %>%
  summarize(unique_caterpillars = n_distinct(CAT_sp)) %>%
  arrange(locality, PAR_sp)

# Calculate average host specificity for caterpillars at each locality
avg_cat_specificity <- cat_specificity %>%
  group_by(locality) %>%
  summarize(mean_specificity = mean(unique_plants),
            sd_specificity = sd(unique_plants))

# Calculate average host specificity for parasitoids at each locality
avg_par_specificity <- par_specificity %>%
  group_by(locality) %>%
  summarize(mean_specificity = mean(unique_caterpillars),
            sd_specificity = sd(unique_caterpillars))

# Print the results
print(cat_specificity)
print(par_specificity)
print(avg_cat_specificity)
print(avg_par_specificity)


# Example results for caterpillar host specificity
cat_specificity

par_specificity

avg_par_specificity_loc <- avg_par_specificity %>%
  summarize(mean_specificity_par = mean(mean_specificity),
            sd_specificity_par = mean(sd_specificity))

avg_cat_specificity_loc <- avg_cat_specificity %>%
  summarize(mean_specificity_cat = mean(mean_specificity),
            sd_specificity_cat = mean(sd_specificity))




