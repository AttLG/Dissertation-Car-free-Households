##############  Dissertation  ##############
# Date: 05/08/2025
# Desc: Descriptive stats

############################################
library(sf)
library(stringr)
library(tidyr)

####################################################################################################
###                              Counts                                  ##
####################################################################################################
nrow(shp_data)
table(shp_data$urban_rural)
table(shp_data$ptal_score)

####################################################################################################
###                              Log Transformed                                  ##
####################################################################################################
# spread
summary(shp_data$log_social_leisure)
summary(shp_data$log_food_shopping)
summary(shp_data$log_work)

# sd
sd(shp_data$log_social_leisure)
sd(shp_data$log_food_shopping)
sd(shp_data$log_work)

####################################################################################################
###                              Spread for continuous and count                                   ##
####################################################################################################
# numeric variables 
num_vars_raw <- c(
  "HPIndex22_rel",
  "prop_age20_34",
  "prop_male_T1",
  "prop_bp_abroad",
  "prop_addr_change",
  "avg_household_size",
  "prop_lone_parent_families",
  "prop_built_2016plus",
  "prop_no_heating",
  "prop_has_renewables",
  "prop_volunteers",
  "prop_managers_prof",
  "prop_agriculture",
  "prop_long_commute",
  "prop_disabled",
  "prop_carers",
  "prop_good_health",
  "pop_density",
  "food_shopping",
  "social_leisure",
  "work",
  "primary_edu",
  "secondary_edu",
  "tertiary_edu"
)

spread_table <- sapply(num_vars_raw, function(var) {
  x <- shp_data_nogeom[[var]]
  q <- quantile(x, probs = c(0.25, 0.75), names = FALSE, na.rm = TRUE)
  c(
    Min = min(x, na.rm = TRUE),
    Q1 = q[1],
    Median = median(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Q3 = q[2],
    Max = max(x, na.rm = TRUE)
  )
})

# convert to df
spread_table_df <- as.data.frame(t(spread_table))
spread_table_df$Variable <- rownames(spread_table_df)
spread_table_df <- spread_table_df[, c("Variable", "Min", "Q1", "Median", "Mean", "Q3", "Max")]

write.csv(spread_table_df, "output/numeric_predictor_summary.csv", row.names = FALSE)
print(spread_table_df)

####################################################################################################
###                              SD for continuous and count                                      ##
####################################################################################################
# Calculate SDs
sd_table <- sapply(num_vars_raw, function(var) sd(shp_data_nogeom[[var]], na.rm = TRUE))

# Convert to df
sd_table_df <- data.frame(
  Variable = num_vars_raw,
  SD = sd_table
)

write.csv(sd_table_df, "output/numeric_predictor_sds.csv", row.names = FALSE)
print(sd_table_df)




