##############  Dissertation  ##############
# Date: 09/07/2025
# Desc: Variable corr checks for Neg Binomial

############################################
library(readr)
library(tidyverse)
library(dplyr)
library(sf)


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
load_data <- read.csv("data/data_for_checks.csv")

predictors_corr <- c(
  "dep_index_scaled",           
  "prop_age20_34",
  "mean_age_index",
  "prop_male_T1",
  "prop_bp_abroad",
  "prop_addr_change",
  "avg_household_size_scaled",     
  "prop_apt_flat",
  "prop_built_2016plus",
#  "prop_homeowners",
  "prop_no_heating",
  "prop_has_renewables",
  "prop_volunteers",
  "prop_managers_prof",
  "prop_agriculture",
  "prop_long_commute",
  "prop_disabled",
  "prop_carers",
#  "health_index",
  "prop_good_health",
  "pop_density_scaled",           
  "urban_rural",
  "ptal_score",
  "log_food_shopping_scaled",      
  "log_social_leisure_scaled",     
#  "log_friends_family_scaled",     
  "log_tertiary_edu_scaled",       
  "log_primary_edu_scaled",        
  "log_work_scaled",               
  "log_secondary_edu_scaled" ,     

#   "prop_work_from_home",
#   "prop_no_or_primary",
#   "prop_unemployed"
#  "avg_family_size",
  "prop_lone_parent_families"
#  "prop_one_person_hh",
#  "prop_couple_with_children",
#  "prop_multi_family_units",
#   "prop_singles",
#  "prop_married"
)


###################################################################################################
###                                        Multicollinearity                                     ##
###################################################################################################
# keep numeric only
corr_data <- shp_data %>%
  st_drop_geometry() %>%
  select(all_of(predictors_corr)) %>%
  select(where(is.numeric)) %>%
  select(where(~ n_distinct(.) > 10))    

# correlation matrix
cor_matrix <- cor(corr_data, use = "complete.obs")

# # Plot correlation matrix
# library(ggcorrplot)
# ggcorrplot(
#   cor_matrix,
#   type = "lower",
#   lab = FALSE,
#   lab_size = 2,
#   colors = c("#2A9D8F", "white", "#E76F51"),  # Brand colours
#   title = "Correlation Matrix of Predictors"
# ) +
#   theme_brand()

# list correlations
library(reshape2)
# Remove redundant pairs (lower triangle and diagonal)
cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
# Convert to long format
cor_all <- melt(cor_matrix, na.rm = TRUE)
# Rename columns
cor_all <- cor_all %>%
  rename(var1 = Var1, var2 = Var2, correlation = value) %>%
  arrange(desc(abs(correlation)))
# View the first few rows
head(cor_all)

write.csv(cor_all, file = "output/Multicollinearity.csv")


###################################################################################################
###                                     VIF Diagnostics                                          ##
###################################################################################################
library(car)

vif_data <- shp_data %>%
  st_drop_geometry() %>%
  select(car_free_count, all_of(predictors_corr)) %>%
  mutate(across(where(is.logical), as.integer)) %>%   # just in case
  select(car_free_count, where(is.numeric)) %>%       # numeric only
  # keep columns with enough unique values and non-zero variance
  select(
    car_free_count,
    where(~ dplyr::n_distinct(.) > 10 && stats::sd(., na.rm = TRUE) > 0)
  )

# what was kept
names(vif_data)

# fit model for VIF
lm_full <- lm(car_free_count ~ ., data = na.omit(vif_data))

v <- car::vif(lm_full)













