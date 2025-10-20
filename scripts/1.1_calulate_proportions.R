##############  Dissertation  ##############
# Date: 19/06/2024
# Desc: Include/exclude variables and convert to proportions

############################################
library(readr)
library(tidyverse)

rm(list = ls())
####################################################################################################
###                                      Import data                                              ##
####################################################################################################
saps_temp <- read.csv("data/saps_2022_cleaned_30pc_20hh.csv")

summary(saps_temp$HPIndex22_rel)

hist(saps_2022$T1_1AGETT)
summary(saps_2022$T1_1AGETT)

hist(saps_2022$T15_1_TC)
summary(saps_2022$T15_1_TC)

####################################################################################################
###                                   Calculate Proportions                                       ##
####################################################################################################
####################################################################################################
###                              T1_1 - Calculate Age & Male                                ##
####################################################################################################

saps_2022 <- load_data %>%
  mutate(total_pop_t1 = T1_1AGETT,
    prop_age20_34 = (T1_1AGE20_24T + T1_1AGE25_29T + T1_1AGE30_34T) / T1_1AGETT)

saps_2022 <- saps_2022%>%
  mutate (prop_male_T1 = T1_1AGETM / T1_1AGETT)


####################################################################################################
###                            T1_2 Calculate single proportion                                ##
####################################################################################################

saps_2022 <- saps_2022 %>%
  mutate(prop_singles = T1_2SGLT / T1_2T,
         prop_married = T1_2MART / T1_2T)

####################################################################################################
###                             T3_1 Calculate Birthplace & Mobility                               ##
####################################################################################################
saps_2022 <- saps_2022 %>%
  mutate(# Proportion Birthplace outside Ireland
          prop_bp_abroad = (1 - (T2_1IEBP / T2_1TBP)),
          # proportion at address in different county or abroad
          prop_addr_change = (T2_3EI + T2_3OI) / T2_3T
        )


####################################################################################################
###                               Remove T3 - Irish Language                                      ##
####################################################################################################

## saps_2022_filtered <- saps_2022_filtered[ , !grepl("^T3", names(saps_2022_filtered))]

####################################################################################################
###                                       T4 - Families                                           ##
####################################################################################################

# avg family size
saps_2022 <- saps_2022 %>% mutate( avg_family_size = T4_1_TP / T4_1_TF )

# single parent hh
saps_2022 <- saps_2022 %>%
  mutate(prop_lone_parent_families = (T4_3FOPMCT + T4_3FOPFCT) / T4_2_TCT)


####################################################################################################
###                                  T5 - Private Households                                     ##
####################################################################################################

# total number of households
saps_2022 <- saps_2022 %>% mutate(total_hh_T5 = T5_1T_H)

# household type
saps_2022 <- saps_2022 %>%
  mutate(prop_one_person_hh = T5_1OP_H / T5_1T_H,
          prop_couple_with_children = (T5_1MCC_H + T5_1CCC_H) / T5_1T_H,
          # Multigenerational / complex households
          prop_multi_family_units = (T5_1GETFU_H + T5_1GENP_H) / T5_1T_H
        )

# Average Household Size
  saps_2022 <- saps_2022 %>%
    mutate( avg_household_size = T5_2_TP / T5_2_TH )

  
####################################################################################################
###                                       T6 - Housing                                            ##
####################################################################################################
  
# housing type
saps_2022 <- saps_2022 %>%
  mutate(prop_apt_flat = T6_1_FA_H / T6_1_TH )
  
# building age
saps_2022 <- saps_2022 %>%
    mutate( prop_built_2016plus = T6_2_16LH / (T6_2_TH - T6_2_NSH) )
  
# homeowners
saps_2022 <- saps_2022 %>%
  mutate(prop_homeowners  = (T6_3_OMLH + T6_3_OOH) / (T6_3_TH - T6_3_NSH))

# no central heating
saps_2022 <- saps_2022 %>%
  mutate(prop_no_heating = T6_5_NCH / (T6_5_T - T6_5_NS))

# uses renewable energies 
saps_2022 <- saps_2022 %>%
  mutate(prop_has_renewables = T6_10_RE / (T6_10_T - T6_10_NS))


####################################################################################################
###                                       T7 - Volunteers                                        ##
####################################################################################################

saps_2022 <- saps_2022 %>%
  mutate(prop_volunteers = T7_1_VOL / T1_1AGETT)

####################################################################################################
###                           T8 - Principal Status (employment)                                  ##
####################################################################################################

# unemployed
saps_2022 <- saps_2022 %>%
  mutate(prop_unemployed = (T8_1_STUT + T8_1_LTUT + T8_1_LFFJT) / T8_1_TT)


####################################################################################################
###                        T9 - Social Class and Socio-Economic Group                             ##
####################################################################################################

# From Max Project (adjusted)
# the proportion of households within socio-economic groups A or B (employers or managers & higher professionals).
saps_2022 <- saps_2022 %>%
  mutate(prop_managers_prof = (T9_2_HA + T9_2_HB) / T9_2_HT,
         prop_agriculture = (T9_2_HI + T9_2_HJ) / T9_2_HT
         )

####################################################################################################
###                                      T10 - Education                                         ##
####################################################################################################

# no edu, or just primary
saps_2022 <- saps_2022 %>%
  mutate(prop_no_or_primary = (T10_4_NFT + T10_4_PT) / (T10_4_TT - T10_4_NST) )

####################################################################################################
##                     T11 -  Commuting  , Working from Home                       ##
####################################################################################################

# commute time 
saps_2022 <- saps_2022 %>%
  mutate( prop_long_commute   = (T11_3_D5 + T11_3_D6) / (T11_3_T - T11_3_NS) )

# work from home
saps_2022 <- saps_2022 %>%
  mutate( prop_work_from_home   = T11_4_WFH / (T11_4_T - T11_4_NS))


####################################################################################################
##                   T12 - Disability, Carers, General Health and Smoking                         ##
####################################################################################################

# disability
saps_2022 <- saps_2022 %>%
  mutate(prop_disabled = T12_1_T / T1_1AGETT)

# carers
saps_2022 <- saps_2022 %>%
  mutate(prop_carers = T12_2_T / T1_1AGETT)

# health
saps_2022 <- saps_2022 %>%
  mutate(prop_good_health = (T12_3_FT + T12_3_GT + T12_3_VGT) / (T12_3_TT - T12_3_NST))

####################################################################################################
##                                       T13 - Occupations                                        ##
####################################################################################################


####################################################################################################
##                                          T14 - Industries                                     ##
####################################################################################################

####################################################################################################
##               T15 - Motor Car Availability, PC Ownership and Internet Access                   ##
####################################################################################################
 
## Households that did not state their car ownership status were excluded. 
# Car Ownership 
saps_2022 <- saps_2022 %>%
  mutate(
    car_total_hh = (T15_1_TC - T15_1_NSC),
    car_free_prop = T15_1_NC / car_total_hh,
    car_free_count = T15_1_NC
    )
  

###################################################################################################
###                                       Population Density                                     ##
###################################################################################################

saps_2022 <- saps_2022 %>%
  mutate(pop_density = T1_1AGETT / (AREA / 1e6))  # per km²


###############    density check  ###############    
county_pop_density <- load_data %>%
  group_by(COUNTY) %>%  
  summarise(
    total_pop = sum(T1_1AGETT, na.rm = TRUE),
    total_area_km2 = sum(AREA, na.rm = TRUE) / 1e6,
    density_per_km2 = total_pop / total_area_km2
  )

write.csv(county_pop_density, file = "output/count_pop_density.csv", row.names = FALSE)

###################################################################################################
###                                   Remove Original Variables                                  ##
###################################################################################################

saps_2022_calc <- saps_2022 %>%
  dplyr::select(
    -starts_with("T1_1"),
    -starts_with("T1_2"),
    -starts_with("T2_"),
    -starts_with("T3_"),
    -starts_with("T4_"),
    -starts_with("T5_"),
    -starts_with("T6_"),
    -starts_with("T7_"),
    -starts_with("T8_"),
    -starts_with("T9_"),
    -starts_with("T10_"),
    -starts_with("T11_"),
    -starts_with("T12_"),
    -starts_with("T13_"),
    -starts_with("T14_"),
    -starts_with("T15_")
  )

###################################################################################################
###                                      Check NAs                                              ##
###################################################################################################

table(is.na(saps_2022_calc))

# view nas
saps_with_nas <- saps_2022_calc[!complete.cases(saps_2022_calc), ]
write.csv(saps_with_nas, "output/proportions_nas.csv")

nas_raw <- saps_2022[saps_2022$GEOGID %in% c("267056005/02", "267056007/02"), ]
write.csv(nas_raw, "output/proportions_nas_raw.csv")

# remove from data
saps_2022_clean <- saps_2022_calc[complete.cases(saps_2022_calc), ]
table(is.na(saps_2022_clean))

###################################################################################################
###                                      Final Data                                            ##
###################################################################################################

write.csv(data.frame(column_name = colnames(saps_2022_clean)), "output/column_names.csv", row.names = FALSE)

exclude_vars <- c(
  "GEOGDESC",
  "UR_Category",
  "UR_Category_Desc",
  "AREA",
  "SA_GUID",
  "OBJECT_ID",
  "total_cars_ns",
  "total_hh_t5"
)

# Drop the columns
final_data <- saps_2022_clean %>% dplyr::select(-all_of(exclude_vars))



write.csv(final_data, "data/data_2022_for_merge.csv", row.names = FALSE)


