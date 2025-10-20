##############  Dissertation  ##############
# Date: 06/07/2025
# Desc: Merge SAPS with spatial data & attractions data

############################################
library(readr)
library(tidyverse)
library(sf)


rm(list = ls())

####################################################################################################
###                                      Import SAPS data                                         ##
####################################################################################################
saps_2022 <- read.csv("data/data_2022_for_merge.csv")


####################################################################################################
###                                 Import  Boundaries                                            ##
####################################################################################################
saps_spatial <- st_read("data/Small_Area_Boundaries_2022_subset.gpkg")

# Join census to shapefile
saps_2022_joined <- saps_2022 %>% left_join(saps_spatial, by = c("GUID" = "SA_GUID_2022"))
class(saps_2022_joined)

####################################################################################################
###                                     Factor & Clean                                          ##
####################################################################################################
# Convert urban/rural to factor 
saps_2022_joined$urban_rural <- factor(saps_2022_joined$SA_URBAN_AREA_FLAG,
                                       levels = c(1, 0),
                                       labels = c("urban", "rural"))

table(saps_2022_joined$SA_URBAN_AREA_FLAG)
table(saps_2022_joined$urban_rural)

# write.csv(names(saps_2022_joined), "output/column_names.csv", row.names = FALSE)

my_vars <- c(
  "SA_URBAN_AREA_FLAG",
  "SA_GEOGID_2022",
  "COUNTY_ENGLISH",
  "SA_URBAN_AREA_NAME"
  )

# Drop  columns
saps_2022_clean <- saps_2022_joined %>% dplyr::select(-all_of(my_vars))


####################################################################################################
###                                         Attractions Data                                      ##
####################################################################################################
attr_data <- read.csv("data/csa_level_attractions.csv")

# Join 
saps_2022_attr <- saps_2022_clean %>% left_join(attr_data, by = c("GEOGID" = "area_id"))

# convert to spatial
saps_2022_attr_sf <- st_as_sf(saps_2022_attr)

# Save as GeoPackage 
st_write(saps_2022_attr_sf, "data/data_for_ptal_merge.gpkg")








