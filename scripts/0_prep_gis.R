##############  Dissertation  ##############
# Date: 19/06/2025
# Desc: Explore & Prep GIS SAP data
############################################
library(readr)
library(tidyverse)
library(sf)

rm(list = ls())

###################################################################################################
###                                   Read Shape File                                            ##
###################################################################################################
st_layers("data/Small_Area_Boundaries_2022_Ungen.gpkg")
shape_data_2022 <- st_read("data/Small_Area_Boundaries_2022_Ungen.gpkg", layer = "SMALL_AREA_2022")
summary(shape_data_2022)

###################################################################################################
###                                      Urban/Rural                                            ##
###################################################################################################
table(shape_data_2022$SA_URBAN_AREA_FLAG)

####################################################################################################
###                                    Extract Col Names                                          ##
####################################################################################################

col_names <- data.frame(Column_Names = colnames(shape_data_2022))

write.csv(col_names, "output\\column_names_shape.csv", row.names = TRUE)

###################################################################################################
###                                         Checks                                              ##
###################################################################################################
# CRS
st_crs(subset_shape_data)

st_geometry(subset_shape_data)
plot(st_geometry(subset_shape_data)) 

###################################################################################################
###                                       Subset                                                ##
###################################################################################################
subset_shape_data <- shape_data_2022 %>%
  select(SA_GUID_2022, SA_GEOGID_2022, SA_URBAN_AREA_FLAG, SA_URBAN_AREA_NAME, COUNTY_ENGLISH, SHAPE)

st_write(subset_shape_data, "data/Small_Area_Boundaries_2022_subset.gpkg")



















