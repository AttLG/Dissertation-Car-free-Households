##############  Dissertation  ##############
# Date: 06/07/2025
# Desc: PTAL data

# The Public Transport Accessibility Level (PTAL) analysis combines the walk or cycle journey time to a Public Transport stop with the level of service at that stop. 
# It gives an idea of how well connected an area is to Public Transport services.

############################################
library(readr)
library(tidyverse)
library(sf)
library(ggplot2)


rm(list = ls())

####################################################################################################
###                                   Import PTAL data                                            ##
####################################################################################################
# List layers
# layers <- st_layers("data/PTAL_December_2024.gdb")
# print(layers)
# Read
ptal_data <- st_read("data/PTAL_December_2024.gdb", layer = "PTAL_December_2024")

# check crs 
st_crs(ptal_data)

# unique(ptal_data$NTA_PTAL)
# summary(ptal_data)

####################################################################################################
###                                   Import SAPS data                                            ##
####################################################################################################
# List layers
# layers <- st_layers("data/data_for_mapping.gpkg")
# print(layers)
# Read 
saps_data <- st_read("data/data_for_ptal_merge.gpkg", layer = "data_for_ptal_merge")

####################################################################################################
###                                   Calculate Service Mode                                     ##
####################################################################################################
# match CRS
ptal_data <- st_transform(ptal_data, st_crs(saps_data))

# spatial intersection
ptal_intersect <- st_intersection(saps_data, ptal_data)

# count categories
ptal_summary <- ptal_intersect %>%
  group_by(GEOGID, NTA_PTAL) %>%    
  summarise(count = n(), .groups = "drop")

# category with highest count 
ptal_mode <- ptal_summary %>%
  group_by(GEOGID) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE)

# Join to saps / No Service
saps_ptal <- saps_data %>%
  left_join(ptal_mode %>%
              st_drop_geometry() %>%
              dplyr::select(GEOGID, NTA_PTAL),
            by = "GEOGID") %>%
  mutate(NTA_PTAL = ifelse(is.na(NTA_PTAL), "No Service", NTA_PTAL))

table(saps_ptal$NTA_PTAL)

# Save as gpkg
st_write(saps_ptal, "data/data_for_mapping.gpkg")

# save as df
saps_ptal_df <- st_drop_geometry(saps_ptal)
write.csv(saps_ptal_df, file = "data/data_for_checks.csv", row.names = FALSE)


