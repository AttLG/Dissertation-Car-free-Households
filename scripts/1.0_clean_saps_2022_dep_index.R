##############  Dissertation  ##############
# Date: 18/06/2025
# Desc: Explore & Prep 
#       1. Census SAP statistics
#       3. Area Type & DP Index
############################################
library(readr)
library(tidyverse)

rm(list = ls())

####################################################################################################
###                                           Theme                                              ##
####################################################################################################
brand_cols <- c(
  dark_teal  = "#264653",
  green_teal = "#2A9D8F",
  sand_yellow = "#E9C46A",
  orange     = "#F4A261",
  soft_red   = "#E76F51"
)

# ---- Theme ----
theme_brand <- function() {
  theme_minimal(base_family = "Segoe UI") +
    theme(
      text             = element_text(colour = brand_cols["dark_teal"]),
      plot.title       = element_text(size = 20, face = "bold", color = "#264653"),
      plot.subtitle = element_text(size = 18, color = "#264653"),
      strip.text = element_text(size = 14, color = "#264653"),
      axis.title = element_text(size = 16, color = "#264653"),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(colour = "#E0E0E0", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA)
    )
}

# ---- Apply Globally ----
theme_set(theme_brand())

####################################################################################################
###                                      Import data                                              ##
####################################################################################################
load_data <- read_csv("data/SAPS_2022_raw_UR.csv")
# remove totals row
saps_2022 <- load_data %>% filter(row_number() <= n()-1)

## check for missing values
sum(is.na(saps_2022))

####################################################################################################
###                                       Area Types                                              ##
####################################################################################################
area_types <- read_csv("data/SSAP_2022_AreaTypes.csv")

myvars <- c(
  "BUA_ID",
  "BUA_NAME",
  "AREA_TY_ID"
)

area_types <- area_types[ , !(names(area_types) %in% myvars)]

saps2022_areatypes <- saps_2022 %>% left_join(area_types, by = c("GEOGID" = "SMALL_AREA"))

####################################################################################################
###                                    Deprivation Index                                         ##
####################################################################################################

dep_index <- read_csv("data/2022_dep_index_updated_subset.csv")

saps_2022 <- saps2022_areatypes %>% left_join(dep_index, by = c("GEOGID" = "SA_PUB2022"))

rm(area_types)
rm(saps2022_areatypes)
rm(dep_index)

####################################################################################################
###                                 Rename / Calculate Fields                                    ##
####################################################################################################
sap_temp <- saps_2022 %>%
  mutate(total_hh_t5 = T5_1T_H,
          total_cars_raw = T15_1_TC,
         total_cars_ns = T15_1_TC - T15_1_NSC,
         prop_car_response_raw = T15_1_TC / T5_1T_H,
         prop_car_response_ns = (T15_1_TC - T15_1_NSC) / T5_1T_H
  )

####################################################################################################
###                                       Remove NAs                                              ##
####################################################################################################
table((is.na(sap_temp)))

# write nas
sap_temp_na <- sap_temp[!complete.cases(sap_temp), ]

my_vars <-  c("COUNTY"
              , "LA_NAME"
              , "AREA"
              , "AREA_TYPE"
              , "UR_Category_Desc"
              , "HPIndex22_rel"
              , "total_hh_t5"
              , "total_cars_raw"
              , "total_cars_ns"
              , "prop_car_response_raw"
              , "prop_car_response_ns"
              )

saps_2022_na <- sap_temp_na %>% dplyr::select(all_of(my_vars))

write.csv(saps_2022_na, "output/saps_2022_nas.csv", row.names = TRUE)

# remove from data
sap_temp <- sap_temp[complete.cases(sap_temp), ]




####################################################################################################
###                         Remove Areas Less than 30pc response T15                             ##
####################################################################################################

# remove from data
saps_2022_30pc <- sap_temp[sap_temp$prop_car_response_ns >= .3, ]

# record excluded
saps2022_less_30pc <- sap_temp[sap_temp$prop_car_response_ns < .3, ]
saps2022_less_30pc <- saps2022_less_30pc %>% select(all_of(my_vars))

#write.csv(saps2022_less_30pc, file = "output/saps_2022_less_30pc.csv")

# test number of hh
summary(saps_2022_30pc$total_hh_t5)
hist(saps_2022_30pc$total_hh_t5)

####################################################################################################
###                                 Remove Areas Less than 20 HH                                ##
####################################################################################################

## remove from data
saps_2022_30pc_20 <- saps_2022_30pc[saps_2022_30pc$total_hh_t5 > 20, ]

## record excluded
saps_temp_20 <- saps_2022_30pc[saps_2022_30pc$total_hh_t5 <= 20, ]
saps_temp_20 <- saps_temp_20 %>% select(all_of(my_vars))

write.csv(saps_temp_20, file = "output/saps_2022_less_30pc_20_hh.csv")


#####################################################################################################
###                                     Data for Analysis                                         ##
#####################################################################################################

write.csv(saps_2022_30pc_20, "data/saps_2022_cleaned_30pc_20hh.csv", row.names = FALSE)






####################################################################################################
###                                    Visualisations                                            ##
####################################################################################################

########################         Total Households Table 5         ######################## 
summary(saps_2022$T5_1T_H)
summary(saps_2022_30pc_20$total_hh_t5)

ggplot(saps_2022_30pc_20, aes(x = T5_1T_H)) +
  geom_histogram(
    bins = 40,
    fill = "#2A9D8F",
    colour = "#264653"
  ) +
  labs(
    title = "Distribution of Households per Small Area (2022)",
    x = "Total Households per Small Area",
    y = NULL
  ) +
  theme_brand()


# boxplot raw data
ggplot(saps_2022, aes(y = T5_1T_H)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "red2", outlier.shape = 1) +
  labs(
    title = "Households per Small Area (2022)",
    y = "Number of HH",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )



########################         Total cars         ########################         

summary(saps_2022$T15_1_TC)
summary(sap_temp$total_cars_raw)
summary(sap_temp$total_cars_ns)

# < 50 households
# less_30pc_car_response <- sap_temp[sap_temp$total_cars_ns < 30, ]

# less_50_car_response <- less_50_car_response %>%
#   select(all_of(my_vars))
# 
# write.csv(less_50_car_response, file = "output/car_response_less_50.csv")

summary(saps_2022_30pc_20$T15_1_NC)
# Histogram
ggplot(saps_2022_30pc_20, aes(x = T15_1_NC)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of No Car Households per Small Area (2022)",
    x = "Count NC Households per Small Area",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
  )


# boxplot 
ggplot(sap_temp, aes(y = total_cars)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "red2", outlier.shape = 1) +
  labs(
    title = "Total Cars",
    y = NULL,
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


########################       Car Response (T15)         ######################## 

summary(sap_temp$prop_car_response_ns)
sap_car_response_ns <- as.data.frame(table(round(sap_temp$prop_car_response_ns, digits = 2)))

response_less_60pc <- sap_temp[sap_temp$prop_car_response_ns < .6, ]
response_less_60pc <- response_less_60pc %>%
  select(all_of(my_vars))

write.csv(response_less_60pc, file = "output/T15_response_less_60pc.csv")


hist(response_less_30pc$HPIndex22_rel)
hist(response_less_40pc$HPIndex22_rel)
hist(response_less_50$HPIndex22_rel)
hist(response_less_60pc$HPIndex22_rel)

############################          t15 response          ############################
ggplot(sap_temp, aes(x = prop_car_response_ns)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Proportion of Households that responded to T15 questions",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
  )


############################          t15 response > 30pc         ############################          
car_response_30 <- sap_temp[sap_temp$prop_car_response_ns >= .3 , ]

summary(car_response_30$total_hh_t5)

ggplot(car_response_30, aes(y = total_hh_t5)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "red2", outlier.shape = 1) +
  labs(
    title = "Proportion of Household that responded to T15 questions",
    y = NULL,
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggplot(saps_2022_30pc_20, aes(y = total_hh_t5)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "red2", outlier.shape = 1) +
  labs(
    title = "Proportion of Household that responded to T15 questions",
    y = NULL,
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

############################          t15 response > 40pc        ############################ 
car_response_40 <- sap_temp[sap_temp$prop_car_response_ns >= .4 , ]

summary(car_response_40$total_hh_t5)

ggplot(car_response_40, aes(y = total_hh_t5)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "red2", outlier.shape = 1) +
  labs(
    title = "Proportion of Household that responded to T15 questions",
    y = NULL,
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

############################          t15 response > 50pc        ############################ 

car_response_50 <- sap_temp[sap_temp$prop_car_response_ns >= .5 , ]

summary(car_response_50$total_hh_t5)

# boxplot t15 response > 50
ggplot(car_response_50, aes(y = prop_car_response_ns)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "red2", outlier.shape = 1) +
  labs(
    title = "Proportion of Household that responded to T15 questions",
    y = NULL,
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


############################          t15 response > 50pc        ############################ 

car_response_60 <- sap_temp[sap_temp$prop_car_response_ns >= .6 , ]

summary(car_response_60$total_hh_t5)

# boxplot t15 response > 60
ggplot(car_response_60, aes(y = total_hh_t5)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "red2", outlier.shape = 1) +
  labs(
    title = "Households that responded to T15 questions (>=60pc)",
    y = NULL,
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

############################          t15 response > 60         ############################
# hist 
ggplot(saps_2022_60pc, aes(x = prop_car_response_ns)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "60pc or More Households responded to T15 questions",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
  )

# boxplot > 60pc
ggplot(saps_2022_60pc, aes(y = total_hh_t5)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "red2", outlier.shape = 1) +
  labs(
    title = "Households per Small Area (2022)",
    y = "Number of HH",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )







