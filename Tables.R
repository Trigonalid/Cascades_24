# Load necessary libraries
library(vegan)
library(broom)
library(dplyr)

# Example data
data(dune)
data(dune.env)

# Convert variables to numeric if they are not already
if (!is.numeric(dune.env$Moisture)) {
  dune.env$Moisture <- as.numeric(as.character(dune.env$Moisture))
}
if (!is.numeric(dune.env$A1)) {
  dune.env$A1 <- as.numeric(as.character(dune.env$A1))
}

# Create distance matrices
dist1 <- vegdist(dune)
dist2 <- vegdist(dune.env$A1, method = "euclidean")
dist3 <- vegdist(dune.env$Moisture, method = "euclidean")

# Perform Mantel tests
mantel_result1 <- mantel(dist1, dist2)
mantel_result2 <- mantel(dist1, dist3)

# Tidy the results
tidy_mantel1 <- tidy(mantel_result1) %>% mutate(test = "Mantel Test 1")
tidy_mantel2 <- tidy(mantel_result2) %>% mutate(test = "Mantel Test 2")

# Combine the results into one table
results <- bind_rows(tidy_mantel1, tidy_mantel2)

# View the results
print(results)

