##############  Dissertation  ##############
# Date: 25/07/2025
# Desc: Visualise relationships 

############################################
library(readr)
library(tidyverse)
library(ggplot2)
library(showtext)

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

# ########### Remove large values ##########
# # - Urban AND pop_density > 100000
# # - Rural AND pop_density > 2999
# saps_cleaned <- load_data %>%
#   filter(
#     !(urban_rural == "urban" & pop_density > 100000),
#     !(urban_rural == "rural" & pop_density > 2999)
#   )



####################################################################################################
###                                           Factors                                             ##
####################################################################################################
load_data$ptal_score <- factor(load_data$NTA_PTAL,
                                  levels = c("No Service", 
                                             "Low Level of Service", 
                                             "Medium Level of Service", 
                                             "Medium - High Level of Service", 
                                             "High Level of Service"),
                                  ordered = TRUE)

load_data$urban_rural <- factor(load_data$urban_rural,
                                levels = c("rural", "urban"),
                                ordered = TRUE)

load_data$area_type <- factor(load_data$AREA_TYPE,
                              levels = c("Rural", 
                                         "Town or Village", 
                                         "Local Centre", 
                                         "Key Town", 
                                         "Regional Centre", 
                                         "City"),
                              ordered = TRUE)

class(load_data$ptal_level)
class(load_data$urban_rural)
class(load_data$area_type)


# # ########## numeric for correlation
#  load_data$ptal_level_num <- as.numeric(load_data$ptal_level)
#  load_data$urban_rural_num   <- as.numeric(load_data$urban_rural)
#  load_data$area_type_num     <- as.numeric(load_data$AREA_TYPE)
# 
# 
# # Spearman Correlations
# library(broom)
# 
# results <- tibble(
#   variable = c("ptal_level", "urban_rural", "area_type"),
#   correlation = c(
#     cor.test(as.numeric(load_data$ptal_level), load_data$car_free_count, method="spearman")$estimate,
#     cor.test(as.numeric(load_data$urban_rural), load_data$car_free_count, method="spearman")$estimate,
#     cor.test(as.numeric(load_data$area_type), load_data$car_free_count, method="spearman")$estimate
#   ),
#   p_value = c(
#     cor.test(as.numeric(load_data$ptal_level), load_data$car_free_count, method="spearman")$p.value,
#     cor.test(as.numeric(load_data$urban_rural), load_data$car_free_count, method="spearman")$p.value,
#     cor.test(as.numeric(load_data$area_type), load_data$car_free_count, method="spearman")$p.value
#   )
# )
# 
# results


###############       Visualise       ###############       

# Car-free Households by PTAL Level
p_ptal <- ggplot(load_data, aes(x = ptal_level, y = car_free_count, fill = ptal_level)) +
  geom_boxplot(outlier.shape = NA, colour = brand_cols["dark_teal"]) +
  geom_jitter(alpha = 0.3, width = 0.2, colour = brand_cols["soft_red"]) +
  scale_fill_brand() +
  labs(title = "Car-free Households by PTAL Level",
       x = "PTAL Level", 
       y = "Car-free Count") +
  theme_brand() +
  theme(legend.position = "none")


# Car-free hh by Urban/Rural 
p_urban_rural <- ggplot(load_data, aes(x = urban_rural, y = car_free_count, fill = urban_rural)) +
  geom_boxplot(outlier.shape = NA, colour = brand_cols["dark_teal"]) +
  geom_jitter(alpha = 0.3, width = 0.2, colour = brand_cols["soft_red"]) +
  scale_fill_manual(values = brand_cols[c("green_teal", "orange")]) + # custom for 2 categories
  labs(title = "Car-free Households by Urban/Rural Classification",
       x = "Urban vs Rural", 
       y = "Car-free Count") +
  theme_brand() +
  theme(legend.position = "none")


# Car-free hh by area Type
p_area_type <- ggplot(load_data, aes(x = area_type, y = car_free_count, fill = area_type)) +
  geom_boxplot(outlier.shape = NA, colour = brand_cols["dark_teal"]) +
  geom_jitter(alpha = 0.3, width = 0.2, colour = brand_cols["soft_red"]) +
  scale_fill_brand() +
  labs(title = "Car-free Households by Area Type",
       x = "Area Type", 
       y = "Car-free Count") +
  theme_brand()



p_ptal
p_urban_rural
p_area_type


####################################################################################################
###                                          Continuous                                           ##
####################################################################################################
####################################################################################################
###                                          Pop density                                          ##
####################################################################################################
ggplot(load_data, aes(x = pop_density, y = car_free_count)) +
  geom_point(alpha = 0.4, colour = brand_cols["dark_teal"]) +
  geom_smooth(method = "loess", se = FALSE, colour = brand_cols["soft_red"]) +
  labs(title = "Population Density vs Care Free Households",
       y = "Car Free Households",
       x = "Population Density")

# by urban / rural
ggplot(load_data, aes(x = pop_density, y = car_free_count)) +
  geom_point(alpha = 0.4, colour = brand_cols["dark_teal"]) +
  geom_smooth(method = "loess", se = FALSE, colour = brand_cols["soft_red"]) +
  facet_wrap(~ urban_rural) +  # replace with your actual column name
  labs(title = "No Car Households vs Population Density",
       subtitle = "by Urban / Rural Areas",
       y = "No Car Households",
       x = "Population Density")


rural_data <- load_data[load_data$urban_rural == "rural" , ]
rural_data <- rural_data[rural_data$pop_density < 2999, ]

summary(rural_data$pop_density)
summary(load_data$pop_density[load_data$urban_rural == "rural"])


ggplot(rural_data, aes(x = pop_density, y = car_free_count)) +
  geom_point(alpha = 0.4, colour = brand_cols["dark_teal"]) +
  geom_smooth(method = "loess", se = FALSE, colour = brand_cols["soft_red"]) +
  labs(title = "No Car Households vs Population Density",
       subtitle = "Rural Areas",
       y = "No Car Households",
       x = "Population Density")



urban_data <- load_data[load_data$urban_rural == "urban", ]
urban_data <- urban_data[urban_data$pop_density < 100000, ]

ggplot(urban_data, aes(x = pop_density, y = car_free_count)) +
  geom_point(alpha = 0.4, colour = brand_cols["dark_teal"]) +
  geom_smooth(method = "loess", se = FALSE, colour = brand_cols["soft_red"]) +
  labs(title = "No Car Households vs Population Density",
       subtitle = "Urban Areas",
       y = "No Car Households",
       x = "Population Density")


#### look at density
load_data %>%
  group_by(urban_rural) %>%
  summarise(
    min_density = min(pop_density),
    median_density = median(pop_density),
    max_density = max(pop_density)
  )

# rural high density
rural_pop_dense <- load_data %>% filter(urban_rural == "rural", pop_density > 500) %>%  
  arrange(desc(pop_density))

write.csv(rural_pop_dense, file = "output/rural_high_density.csv", row.names = FALSE)

# urban low density
urban_dense_low <- load_data %>% filter(urban_rural == "urban", pop_density < 1500) %>%  
  arrange(desc(pop_density))

write.csv(urban_dense_low, file = "output/urban_low_density.csv", row.names = FALSE)

####################################################################################################
###                                          Dep Index                                            ##
####################################################################################################
# needs splines
#scatterplot
ggplot(load_data, aes(x = HPIndex22_rel, y = car_free_count)) +
  geom_point(alpha = 0.4, colour = brand_cols["dark_teal"]) +
  geom_smooth(method = "loess", se = FALSE, colour = brand_cols["soft_red"]) +
  labs(title = "No Car Households vs HPIndex22_rel",
       y = "No Car Households",
       x = "Relative HP Deprivation Index (2022)")


### labels
ggplot(load_data, aes(x = HPIndex22_rel_lab, y = car_free_count)) +
  geom_boxplot(fill = brand_cols["sand_yellow"], colour = brand_cols["dark_teal"]) +
  labs(title = "No Car Households by Deprivation Category",
       x = "Relative HP Deprivation (Categories)",
       y = "No Car Households") +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 14),  # bigger rotated labels
    axis.text.y  = element_text(size = 14),                         # bigger y labels
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title   = element_text(size = 20, face = "bold")
  )


ggplot(load_data, aes(x = HPIndex22_rel, y = car_free_count)) +
  geom_point(alpha = 0.4, colour = brand_cols["dark_teal"]) +
  geom_smooth(method = "loess", se = FALSE, colour = brand_cols["soft_red"]) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "No Car Households vs HP Deprivation Index",
    subtitle = "by Urban and Rural Areas",
    x = "Relative HP Deprivation Index (2022)",
    y = "No Car Households"
  )


####################################################################################################
###                                            Age 20-34                                          ##
####################################################################################################

hist(load_data$prop_age20_34)
summary(load_data$prop_age20_34)

# Scatter plot 
ggplot(load_data, aes(x = prop_age20_34, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E76F51", alpha = 0.2) +
  labs(
    title = "Car-Free Households vs. Age 20–34 Proportion",
    x = "Proportion Age 20–34",
    y = "Car-Free Household Count"
  ) +
  theme_minimal(base_family = "yugothicui") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )


# urban / rural
ggplot(load_data, aes(x = prop_age20_34, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Households vs. Age 20–34 Proportion",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion Age 20–34",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

# Pearson correlation
overall_test <- cor.test(load_data$prop_age20_34, load_data$car_free_count, method = "spearman")
urban_test <- cor.test(load_data$prop_age20_34[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman" )
rural_test <- cor.test(load_data$prop_age20_34[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman" )

overall_test; urban_test; rural_test

#####################   Linear?   #####################
# yes
linear_model <- lm(car_free_count ~ prop_age20_34, data = load_data)
loess_model  <- loess(car_free_count ~ prop_age20_34, data = load_data)

linear_resid <- sd(resid(linear_model)); linear_resid
loess_pred   <- predict(loess_model); 
deviation    <- mean(abs(loess_pred - predict(linear_model))); deviation

deviation / linear_resid
# If deviation < 0.25 * linear_resid, curvature is small → linear is acceptable.



####################################################################################################
###                                       Mean Age Index                                          ##
####################################################################################################

# hist(load_data$mean_age_index)
# summary(load_data$mean_age_index)
# 
# # Scatter plot 
# ggplot(load_data, aes(x = mean_age_index, y = car_free_count)) +
#   geom_point(color = "#264653", size = 2, alpha = 0.7) +
#   geom_smooth(method = "lm", color = "#E76F51", alpha = 0.2) +
#   labs(
#     title = "Car-Free Households vs. Mean Age Index",
#     x = "Mean Age Index",
#     y = "Car-Free Household Count"
#   ) +
#   theme_brand()
# 
# 
# # urban / rural
# ggplot(load_data, aes(x = mean_age_index, y = car_free_count)) +
#   geom_point(color = "#264653", size = 2, alpha = 0.7) +
#   geom_smooth(method = "lm", color = "#E76F51", alpha = 0.2) +
#   facet_wrap(~ urban_rural) +
#   labs(
#     title = "Car-Free Households vs.  Mean Age Index",
#     subtitle = "by Urban / Rural Areas",
#     x = " Mean Age Index",
#     y = "Car-Free Household Count"
#   ) +
#   theme_brand()
# 
# # Pearson correlation
# overall_test <- cor.test(load_data$mean_age_index, load_data$car_free_count, method = "spearman"); overall_test
# urban_test <- cor.test(load_data$prop_age20_34[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman" )
# rural_test <- cor.test(load_data$prop_age20_34[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman" )
# 
# ; urban_test; rural_test
# 
# #####################   Linear?   #####################
# # yes
# linear_model <- lm(car_free_count ~ mean_age_index, data = load_data)
# loess_model  <- loess(car_free_count ~ mean_age_index, data = load_data)
# 
# linear_resid <- sd(resid(linear_model)); linear_resid
# loess_pred   <- predict(loess_model); 
# deviation    <- mean(abs(loess_pred - predict(linear_model))); deviation
# 
# deviation / linear_resid
# # If deviation < 0.25 * linear_resid, curvature is small → linear is acceptable.

####################################################################################################
###                                             Prop Male                                         ##
####################################################################################################
hist(load_data$prop_male_T1)
summary(load_data$prop_male_T1)

# Scatter plot
ggplot(load_data, aes(x = prop_male_T1, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E76F51", alpha = 0.2) +
  labs(
    title = "Car-Free Households vs. Male Proportion",
    x = "Proportion Male",
    y = "Car-Free Household Count"
  ) +
  theme_brand() 



# urban / rural 
ggplot(load_data, aes(x = prop_male_T1, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Households vs. Male Proportion",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion Male",
    y = "Car-Free Household Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )

#####   Pearson's R   #####
overall_test <- cor.test(load_data$prop_male_T1, load_data$car_free_count, method = "pearson" )
urban_test <- cor.test(load_data$prop_male_T1[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "pearson" )
rural_test <- cor.test( load_data$prop_male_T1[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "pearson" )

overall_test; urban_test; rural_test







#####################   Prop Single   #####################

# Scatter plot 
# Use loess to avoid unrealistic negative predictions
ggplot(load_data, aes(x = prop_singles, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E76F51", alpha = 0.2) +
  labs(
    title = "Car-Free Household Count vs. Single Person Household Proportion",
    x = "Proportion Single Person Households",
    y = "Car-Free Household Count"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )


# urban / rural 
ggplot(load_data, aes(x = prop_singles, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Households vs. Single Person Household Proportion",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion Single Person Households",
    y = "Car-Free Households"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )

#####   Pearson's R   #####
overall_test <- cor.test(load_data$prop_singles, load_data$car_free_count, method = "pearson"); overall_test$estimate; overall_test$p.value
urban_test   <- cor.test(load_data$prop_singles[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "pearson"); urban_test$estimate; urban_test$p.value
rural_test   <- cor.test(load_data$prop_singles[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "pearson"); rural_test$estimate; rural_test$p.value

#####################   Linear?   #####################
# yes
linear_model <- lm(car_free_count ~ prop_singles, data = load_data)
loess_model  <- loess(car_free_count ~ prop_singles, data = load_data)

linear_resid <- sd(resid(linear_model)); linear_resid
loess_pred   <- predict(loess_model); 
deviation    <- mean(abs(loess_pred - predict(linear_model))); deviation

deviation / linear_resid
# If deviation < 0.25 * linear_resid, curvature is small → linear is acceptable.



#####################   Birthplace Abroad   #####################

# Overall plot
ggplot(load_data, aes(x = prop_bp_abroad, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#E76F51", alpha = 0.2) +
  labs(
    title = "Car-Free Household Count vs. Proportion Born Abroad",
    x = "Proportion Born Abroad",
    y = "Car-Free Household Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )

# Urban / rural facet plot
ggplot(load_data, aes(x = prop_bp_abroad, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Household Count vs. Proportion Born Abroad",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion Born Abroad",
    y = "Car-Free Household Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )


#####   Pearson's R   #####
overall <- cor.test(load_data$prop_bp_abroad, load_data$car_free_count, method = "pearson"); c(r = overall$estimate, p = overall$p.value)
urban   <- cor.test(load_data$prop_bp_abroad[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "pearson"); c(r = urban$estimate, p = urban$p.value)
rural   <- cor.test(load_data$prop_bp_abroad[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "pearson"); c(r = rural$estimate, p = rural$p.value)



#####################   Address Change   #####################

# Overall 
ggplot(load_data, aes(x = prop_addr_change, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#E76F51", alpha = 0.2) +
  labs(
    title = "Car-Free Household Count vs. Address Change Proportion",
    x = "Proportion Changed Address",
    y = "Car-Free Household Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )

# Urban / rural 
ggplot(load_data, aes(x = prop_addr_change, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Household Count vs. Address Change Proportion",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion Changed Address",
    y = "Car-Free Household Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )

##### Pearson R #####
overall_test <- cor.test(load_data$prop_addr_change, load_data$car_free_count, method = "pearson"); overall_test
urban_test   <- cor.test(load_data$prop_addr_change[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "pearson"); urban_test
rural_test   <- cor.test(load_data$prop_addr_change[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "pearson"); rural_test



#####################   Lone Parents   #####################

# Overall scatter plot
ggplot(load_data, aes(x = prop_lone_parent_families, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#E76F51", alpha = 0.2) +
  labs(
    title = "Car-Free Households vs. Proportion of Lone Parent Families",
    x = "Proportion of Lone Parent Families",
    y = "Car-Free Households"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )

# Urban / rural facet plot
ggplot(load_data, aes(x = prop_lone_parent_families, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Households vs. Proportion of Lone Parent Families",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion of Lone Parent Families",
    y = "Car-Free Households"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )

# -- Pearson correlation
overall_test <- cor.test(load_data$prop_lone_parent_families, load_data$car_free_count, method = "pearson"); overall_test
urban_test   <- cor.test(load_data$prop_lone_parent_families[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "pearson"); urban_test
rural_test   <- cor.test(load_data$prop_lone_parent_families[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "pearson"); rural_test



#####################   Long commute   #####################

# Overall scatter plot
ggplot(load_data, aes(x = prop_long_commute, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E76F51", alpha = 0.2) +
  labs(
    title = "Car-Free Households vs. Proportion of Long Commutes",
    x = "Proportion of Long Commutes",
    y = "Car-Free Household Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )

# Urban / rural 
ggplot(load_data, aes(x = prop_long_commute, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Households vs. Proportion of Long Commutes",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion of Long Commutes",
    y = "Car-Free Household Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#264653"),
    axis.title = element_text(size = 14, color = "#264653"),
    axis.text = element_text(size = 12)
  )

# Spearman correlation
overall_test <- cor.test(load_data$prop_long_commute, load_data$car_free_count, method = "spearman"); overall_test
urban_test   <- cor.test(load_data$prop_long_commute[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_test
rural_test   <- cor.test(load_data$prop_long_commute[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_test


####################################################################################################
###                                      Avg Household Size                                        ##
####################################################################################################
# Overall scatter plot
ggplot(load_data, aes(x = avg_household_size, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E76F51", alpha = 0.2) + 
  labs(
    title = "Car-Free Households vs. Average Household Size",
    x = "Average Household Size",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

# Urban / rural facet plot
ggplot(load_data, aes(x = avg_household_size, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Households vs. Average Household Size",
    subtitle = "by Urban / Rural Areas",
    x = "Average Household Size",
    y = "Car-Free Household Count"
  ) +
  theme_brand()


# Spearman correlations
overall_spearman <- cor.test(load_data$avg_household_size, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$avg_household_size[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$avg_household_size[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman

# Pearson correlations
overall_pearson <- cor.test(load_data$avg_household_size, load_data$car_free_count, method = "pearson"); overall_pearson
urban_pearson   <- cor.test(load_data$avg_household_size[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "pearson"); urban_pearson
rural_pearson   <- cor.test(load_data$avg_household_size[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "pearson"); rural_pearson

#####################   Linear?   #####################
# yes
linear_model <- lm(car_free_count ~ avg_household_size, data = load_data)
loess_model  <- loess(car_free_count ~ avg_household_size, data = load_data)

linear_resid <- sd(resid(linear_model)); linear_resid
loess_pred   <- predict(loess_model); 
deviation    <- mean(abs(loess_pred - predict(linear_model))); deviation

deviation / linear_resid
# If deviation < 0.25 * linear_resid, curvature is small → linear is acceptable.

#####################   Avg Family Size   #####################

hist(load_data$avg_family_size)
## has outliers / incorect data (one row)


# --- Scatterplots ---
ggplot(load_data, aes(x = avg_family_size, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  labs(title = "Car-Free Households vs. Average Family Size",
       x = "Average Family Size", 
       y = "Car-Free Household Count") +
  theme_brand()

# Urban / Rural facet
ggplot(load_data, aes(x = avg_family_size, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Average Family Size",
       subtitle = "by Urban/Rural",
       x = "Average Family Size", 
       y = "Car-Free Household Count") +
  theme_brand()

# --- Spearman’s correlation (1 line each) ---
overall_spearman <- cor.test(load_data$avg_family_size, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$avg_family_size[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$avg_family_size[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman


#####################   Prop Managers / Professionals   #####################

# Scatter with Loess (overall)
ggplot(load_data, aes(x = prop_managers_prof, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  labs(
    title = "Car-Free Households vs. Proportion Managers/Professionals",
    x = "Proportion Managers/Professionals",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

# Scatter by Urban / Rural
ggplot(load_data, aes(x = prop_managers_prof, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Households vs. Proportion Managers/Professionals",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion Managers/Professionals",
    y = "Car-Free Household Count"
  ) +
  theme_brand()


# Spearman correlations
overall_spearman <- cor.test(load_data$prop_managers_prof, load_data$car_free_count, method="spearman", exact=FALSE)
urban_spearman   <- cor.test(load_data$prop_managers_prof[load_data$urban_rural=="urban"], load_data$car_free_count[load_data$urban_rural=="urban"], method="spearman", exact=FALSE)
rural_spearman   <- cor.test(load_data$prop_managers_prof[load_data$urban_rural=="rural"], load_data$car_free_count[load_data$urban_rural=="rural"], method="spearman", exact=FALSE)
# Print results
overall_spearman
urban_spearman
rural_spearman



#####################   Prop Agriculture   #####################

# Scatter with Loess 
ggplot(load_data, aes(x = prop_agriculture, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  labs(
    title = "Car-Free Households vs. Proportion in Agriculture",
    x = "Proportion in Agriculture",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

# Scatter by Urban / Rural
ggplot(load_data, aes(x = prop_agriculture, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Households vs. Proportion in Agriculture",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion in Agriculture",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

# Spearman correlations
overall_spearman <- cor.test(load_data$prop_agriculture, load_data$car_free_count, method = "spearman")
urban_spearman   <- cor.test(load_data$prop_agriculture[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman")
rural_spearman   <- cor.test(load_data$prop_agriculture[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman")
# Print results
overall_spearman
urban_spearman
rural_spearman


#####################   Prop In Apt / Flats  #####################

# --- Scatter plot with loess smoother (overall)
ggplot(load_data, aes(x = prop_apt_flat, y = car_free_count)) +
  geom_point(alpha = 0.5, color = brand_cols["dark_teal"]) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  labs(title = "Car-Free Households vs. Proportion in Apartments/Flats",
       x = "Proportion in Apartments/Flats",
       y = "Car-Free Household Count") +
  theme_brand()

# --- Scatter plot with loess smoother by Urban/Rural
ggplot(load_data, aes(x = prop_apt_flat, y = car_free_count)) +
  geom_point(alpha = 0.5, color = brand_cols["dark_teal"]) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Proportion in Apartments/Flats",
       subtitle = "by Urban / Rural Areas",
       x = "Proportion in Apartments/Flats",
       y = "Car-Free Household Count") +
  theme_brand()

# --- Spearman correlations 
overall_spearman <- cor.test(load_data$prop_apt_flat, load_data$car_free_count, method = "spearman")
urban_spearman   <- cor.test(load_data$prop_apt_flat[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman")
rural_spearman   <- cor.test(load_data$prop_apt_flat[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman")

# --- Print results
overall_spearman
urban_spearman
rural_spearman


#####################   Prop Buildings 2016+  #####################

# ---- Scatter plots with branding ----
ggplot(load_data, aes(x = prop_built_2016plus, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  labs(
    title = "Car-Free Households vs. Proportion Built 2016+",
    x = "Proportion Built 2016+",
    y = "Car-Free Household Count") +
  theme_brand()

ggplot(load_data, aes(x = prop_built_2016plus, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Households vs. Proportion Built 2016+",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion Built 2016+",
    y = "Car-Free Household Count") +
  theme_brand()

# ---- Spearman correlations (one line each) ----
overall_spearman <- cor.test(load_data$prop_built_2016plus, load_data$car_free_count, method = "spearman")
urban_spearman   <- cor.test(load_data$prop_built_2016plus[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman")
rural_spearman   <- cor.test(load_data$prop_built_2016plus[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman")

overall_spearman; urban_spearman; rural_spearman
# --- prop_has_renewables ---

#####################   Prop Renewables  ######################

# Scatterplot with Loess smoother (overall)
ggplot(load_data, aes(x = prop_has_renewables, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], alpha = 0.6) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  labs(title = "Car-Free Households vs. Proportion with Renewable Energy",
       x = "Proportion with Renewable Energy",
       y = "Car-Free Household Count") +
  theme_brand()

# Scatterplot with Loess smoother by Urban/Rural
ggplot(load_data, aes(x = prop_has_renewables, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], alpha = 0.6) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Proportion with Renewable Energy",
       subtitle = "by Urban / Rural Areas",
       x = "Proportion with Renewable Energy",
       y = "Car-Free Household Count") +
  theme_brand()

# Spearman correlations (1-line each)
overall_spearman <- cor.test(load_data$prop_has_renewables, load_data$car_free_count, method = "spearman") 
urban_spearman   <- cor.test(load_data$prop_has_renewables[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman")
rural_spearman   <- cor.test(load_data$prop_has_renewables[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman")

# Print results
overall_spearman; urban_spearman; rural_spearman


#####################   Prop No Heating  ######################

# Overall
ggplot(load_data, aes(x = prop_no_heating, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  labs(
    title = "Car-Free Households vs. Proportion with No Heating",
    x = "Proportion with No Heating",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

# Urban / Rural
ggplot(load_data, aes(x = prop_no_heating, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  facet_wrap(~ urban_rural) +
  labs(
    title = "Car-Free Households vs. Proportion with No Heating",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion with No Heating",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

### --- Spearman correlations ---
overall_spearman <- cor.test(load_data$prop_no_heating, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$prop_no_heating[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$prop_no_heating[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman




#####################   prop_disabled  ######################

### --- Scatter plots with loess smoothing --- ###
ggplot(load_data, aes(x = prop_disabled, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], alpha = 0.5) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  labs(title = "Car-Free Households vs. Proportion with Disabilities",
       x = "Proportion with Disabilities", y = "Car-Free Household Count") +
  theme_minimal(base_family = "Segoe UI") +
  theme(plot.title = element_text(size = 16, face = "bold", color = brand_cols["dark_teal"]))

# urban / rural
ggplot(load_data, aes(x = prop_disabled, y = car_free_count)) +
  geom_point(color = brand_cols["dark_teal"], alpha = 0.5) +
  geom_smooth(method = "loess", color = brand_cols["soft_red"], se = FALSE) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Proportion with Disabilities",
       subtitle = "by Urban / Rural Areas",
       x = "Proportion with Disabilities", y = "Car-Free Household Count") +
  theme_minimal(base_family = "Segoe UI") +
  theme(plot.title = element_text(size = 16, face = "bold", color = brand_cols["dark_teal"]))

### --- Spearman correlations (compact) --- ###
overall_spearman <- cor.test(load_data$prop_disabled, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$prop_disabled[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$prop_disabled[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman


#####################   Prop Carers  ######################

# --- Scatterplot
ggplot(load_data, aes(x = prop_carers, y = car_free_count)) +
  geom_point(alpha = 0.4, color = "#264653") +
  geom_smooth(method = "loess", color = "#E76F51", se = FALSE) +
  labs(title = "Car-Free Households vs. Proportion with Carers",
       x = "Proportion with Carers", 
       y = "Car-Free Household Count") +
  theme_brand()


# Urban / rural
ggplot(load_data, aes(x = prop_carers, y = car_free_count)) +
  geom_point(alpha = 0.4, color = "#264653") +
  geom_smooth(method = "loess", color = "#E76F51", se = FALSE) +
  facet_wrap(~urban_rural) +
  labs(title = "Car-Free Households vs. Proportion with Carers\nby Urban / Rural Areas",
       x = "Proportion with Carers", 
       y = "Car-Free Household Count") +
  theme_brand()

# --- Spearman's correlation tests (compact) ---
overall_spearman <- cor.test(load_data$prop_carers, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$prop_carers[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$prop_carers[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman





#####################     Prop Good Health     #####################

# # --- Scatterplots 
# ggplot(load_data, aes(x = prop_good_health, y = car_free_count)) +
#   geom_point(alpha = 0.4, color = "#264653") +
#   geom_smooth(method = "loess", color = "#E76F51", se = FALSE) +
#   labs(title = "Car-Free Households vs. Proportion in Good Health",
#        x = "Proportion in Good Health", y = "Car-Free Household Count") +
#   theme_brand()
# 
# # Urban / rural
# ggplot(load_data, aes(x = prop_good_health, y = car_free_count)) +
#   geom_point(alpha = 0.4, color = "#264653") +
#   geom_smooth(method = "loess", color = "#E76F51", se = FALSE) +
#   facet_wrap(~urban_rural) +
#   labs(title = "Car-Free Households vs. Proportion in Good Health\nby Urban / Rural Areas",
#        x = "Proportion in Good Health", y = "Car-Free Household Count") +
#   theme_brand()
# 
# 
# # --- Spearman correlations ---
# overall_spearman <- cor.test(load_data$prop_good_health, load_data$car_free_count, method = "spearman") ; overall_spearman
# urban_spearman   <- cor.test(load_data$prop_good_health[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
# rural_spearman   <- cor.test(load_data$prop_good_health[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman
# 



#####################     Health Index     #####################

health_index <- read.csv(file = "data/health_index.csv")
load_data <- load_data %>% left_join(health_index %>% select(GEOGID, health_index), by = "GEOGID")

# --- Scatterplots with loess smooth ---
ggplot(load_data, aes(x = health_index, y = car_free_count)) +
  geom_point(alpha = 0.5, color = "#264653") +
  geom_smooth(method = "loess", color = "#E76F51", se = FALSE) +
  labs(
    title = "Car-Free Households vs. Health Index",
    x = "Health Index",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

ggplot(load_data, aes(x = health_index, y = car_free_count)) +
  geom_point(alpha = 0.5, color = "#264653") +
  geom_smooth(method = "loess", color = "#E76F51", se = FALSE) +
  facet_wrap(~urban_rural) +
  labs(
    title = "Car-Free Households vs. Health Index by Urban / Rural Areas",
    x = "Health Index",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

# --- Spearman correlations (compact form) ---
overall_spearman <- cor.test(load_data$health_index, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$health_index[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$health_index[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman


#####################    Prop Volunteers     #####################


# --- Scatterplot: overall ---
ggplot(load_data, aes(x = prop_volunteers, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E76F51", alpha = 0.3) +
  labs(
    title = "Car-Free Households vs. Proportion of Volunteers",
    x = "Proportion of Volunteers",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

# --- Scatterplot: urban / rural ---
ggplot(load_data, aes(x = prop_volunteers, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E76F51", alpha = 0.3) +
  facet_wrap(~urban_rural) +
  labs(
    title = "Car-Free Households vs. Proportion of Volunteers",
    subtitle = "by Urban / Rural Areas",
    x = "Proportion of Volunteers",
    y = "Car-Free Household Count"
  ) +
  theme_brand()

# --- Spearman’s correlation (1-line each) ---
overall_spearman <- cor.test(load_data$prop_volunteers, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$prop_volunteers[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$prop_volunteers[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman


#####################   Prop WFH   #####################

hist(load_data$prop_work_from_home[load_data$urban_rural == "rural"])
hist(load_data$prop_work_from_home[load_data$urban_rural == "urban"])
summary(load_data$prop_work_from_home)

# --- Scatterplots ---
ggplot(load_data, aes(x = prop_work_from_home, y = car_free_prop)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  labs(title = "Car-Free Households vs. Proportion Working From Home",
       x = "Proportion Working From Home", 
       y = "Car-Free Household Count") +
  theme_brand()

ggplot(load_data, aes(x = prop_work_from_home, y = car_free_prop)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Proportion Working From Home",
       subtitle = "by Urban/Rural",
       x = "Proportion Working From Home", 
       y = "Car-Free Household Count") +
  theme_brand()

# --- Spearman’s correlation (1 line each) ---
overall_spearman <- cor.test(load_data$prop_work_from_home, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$prop_work_from_home[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$prop_work_from_home[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman

#####################   Food Shopping Index   #####################

summary(load_data$food_shopping)
ggplot(load_data, aes(x = food_shopping)) +
  geom_histogram(binwidth = 100, fill = "#264653", color = "white", alpha = 0.7) +
  labs(
    title = "Histogram of Food Shopping Index",
    x = "Food Shopping Index",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()


# Add log-transformed variable (log1p handles zero safely)
load_data <- load_data %>% mutate(food_shopping_log = log1p(food_shopping))

# histogram summary
ggplot(load_data, aes(x = food_shopping_log)) +
  geom_histogram(binwidth = 0.5, fill = "#264653", color = "white", alpha = 0.7) +
  labs(
    title = "Histogram of Log(1 + Food Shopping Index)",
    x = "Log(1 + Food Shopping Index)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()


# scatter plot (log-transformed)
ggplot(load_data, aes(x = food_shopping_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  labs(title = "Car-Free Households vs. Food Shopping Index (log transformed)",
       x = "Log(1 + Food Shopping Index)", 
       y = "Car-Free Household Count") +
  theme_brand()

# urban / rural
ggplot(load_data, aes(x = food_shopping_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5)  +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Food Shopping Index (log transformed)",
       subtitle = "by Urban/Rural",
       x = "Log(1 + Food Shopping Index)", 
       y = "Car-Free Household Count") +
  theme_brand()


## Spearman's Rho ##
overall_test <- cor.test(load_data$food_shopping, load_data$car_free_count, method = "spearman"); overall_test
urban_test   <- cor.test(load_data$food_shopping[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_test
rural_test   <- cor.test(load_data$food_shopping[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_test



#####################  Social Leisure Index   #####################

summary(load_data$social_leisure)
ggplot(load_data, aes(x = social_leisure)) +
  geom_histogram(binwidth = 100, fill = "#264653", color = "white", alpha = 0.7) +
  labs(
    title = "Histogram of Social/Leisure Index",
    x = "Social/Leisure Index",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()

# Add log-transformed variable (log1p handles zero safely)
load_data <- load_data %>% mutate(social_leisure_log = log1p(social_leisure))

summary(load_data$social_leisure_log)
ggplot(load_data, aes(x = social_leisure_log)) +
  geom_histogram(binwidth = 0.25, fill = "#264653", color = "white", alpha = 0.7, boundary = 3) +
  labs(
    title = "Histogram of Social/Leisure Index",
    x = "Social/Leisure Index (log)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()


# --- Scatterplots ---
ggplot(load_data, aes(x = social_leisure_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  labs(title = "Car-Free Households vs. Social Leisure Index (log transformed)",
       x = "Social/Leisure Activities", 
       y = "Car-Free Household Count") +
  theme_brand()

ggplot(load_data, aes(x = social_leisure_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5)  +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Social Leisure Index (log transformed)",
       subtitle = "by Urban/Rural",
       x = "Social/Leisure Activities", 
       y = "Car-Free Household Count") +
  theme_brand()
# 
# # --- Spearman’s correlation (1 line each) ---
# overall_spearman <- cor.test(load_data$social_leisure_log, load_data$car_free_count, method = "spearman"); overall_spearman
# urban_spearman   <- cor.test(load_data$social_leisure_log[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
# rural_spearman   <- cor.test(load_data$social_leisure_log[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman
# 


#####################  Friends Family Index   #####################

summary(load_data$friends_family)
ggplot(load_data, aes(x = friends_family)) +
  geom_histogram(fill = "#264653", color = "white", alpha = 0.7) +
  labs(
    title = "Histogram of Friends/Family Index",
    x = "Friends/Family Index",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()

# Add log-transformed variable (log1p handles zero safely)
load_data <- load_data %>% mutate(friends_family_log = log1p(friends_family))

summary(load_data$friends_family_log)
ggplot(load_data, aes(x = friends_family_log)) +
  geom_histogram(fill = "#264653", color = "white", alpha = 0.7, boundary = 3) +
  labs(
    title = "Histogram of Social/Leisure Index",
    x = "Social/Leisure Index (log)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()

# scatterplots 
ggplot(load_data, aes(x = friends_family_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  labs(title = "Car-Free Households vs. Friends & Family Index (log transformed)",
       x = "Log(1 + Friends & Family Index)", 
       y = "Car-Free Household Count") +
  theme_brand()

# Urban / Rural facet
ggplot(load_data, aes(x = friends_family_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Friends & Family Index (log transformed)",
       subtitle = "by Urban/Rural",
       x = "Log(1 + Friends & Family Index)", 
       y = "Car-Free Household Count") +
  theme_brand()

# --- Spearman’s correlation ---
overall_spearman <- cor.test(load_data$friends_family_log, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$friends_family_log[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$friends_family_log[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman



#####################   tertiary_edu   #####################

ggplot(load_data, aes(x = tertiary_edu)) +
  geom_histogram(fill = "#264653", color = "white", alpha = 0.7) +
  labs(
    title = "Histogram",
    x = "Tertiary Edu Index",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()
summary(load_data$tertiary_edu)

# Add log-transformed variable (log1p handles zero safely)
load_data <- load_data %>% mutate(tertiary_edu_log = log1p(tertiary_edu))

ggplot(load_data, aes(x = tertiary_edu_log)) +
  geom_histogram(fill = "#264653", color = "white", alpha = 0.7, boundary = 3) +
  labs(
    title = "Histogram (log transformed)",
    x = "Tertiary Edu Index (log)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()
summary(load_data$tertiary_edu_log)


# --- Scatterplots ---
ggplot(load_data, aes(x = tertiary_edu_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  labs(title = "Car-Free Households vs. Tertiary Education (log transformed)",
       x = "Log(1 + Tertiary Education Index)", 
       y = "Car-Free Household Count") +
  theme_brand()

# --- Urban / Rural facet ---
ggplot(load_data, aes(x = tertiary_edu_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Tertiary Education (log transformed)",
       subtitle = "by Urban/Rural",
       x = "Log(1 + Tertiary Education Index)", 
       y = "Car-Free Household Count") +
  theme_brand()

# --- Spearman’s correlation  ---
overall_spearman <- cor.test(load_data$tertiary_edu_log, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$tertiary_edu_log[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$tertiary_edu_log[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman


#####################   Primary Edu   #####################

ggplot(load_data, aes(x = primary_edu)) +
  geom_histogram(fill = "#264653", color = "white", alpha = 0.7) +
  labs(
    title = "Histogram",
    x = "Tertiary Edu Index",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()
summary(load_data$primary_edu)

# Add log-transformed variable (log1p handles zero safely)
load_data <- load_data %>% mutate(primary_edu_log = log1p(primary_edu))

ggplot(load_data, aes(x = primary_edu_log)) +
  geom_histogram(fill = "#264653", color = "white", alpha = 0.7, boundary = 3) +
  labs(
    title = "Histogram (log transformed)",
    x = "Tertiary Edu Index (log)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()
summary(load_data$primary_edu_log)



# --- Scatterplots ---
ggplot(load_data, aes(x = primary_edu_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  labs(title = "Car-Free Households vs. Primary Education (log)",
       x = "Primary Education",
       y = "Car-Free Household Count") +
  theme_brand()

ggplot(load_data, aes(x = primary_edu_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Primary Education (log)",
       subtitle = "by Urban/Rural",
       x = "Primary Education",
       y = "Car-Free Household Count") +
  theme_brand()

# --- Spearman’s correlation  ---
overall_spearman <- cor.test(load_data$primary_edu_log, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$primary_edu_log[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$primary_edu_log[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman


#####################   Work (Number of Jobs)   #####################

ggplot(load_data, aes(x = work)) +
  geom_histogram(fill = "#264653", color = "white", alpha = 0.7) +
  labs(
    title = "Histogram",
    x = "Work Index",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()
summary(load_data$work)

# Add log-transformed variable (log1p handles zero safely)
load_data <- load_data %>% mutate(work_log = log1p(work))

ggplot(load_data, aes(x = work_log)) +
  geom_histogram(fill = "#264653", color = "white", alpha = 0.7, boundary = 3) +
  labs(
    title = "Histogram (log transformed)",
    x = "Work Index (log)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme_brand()
summary(load_data$work_log)

# --- Scatterplots (Work) ---

# Overall
ggplot(load_data, aes(x = work_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  labs(title = "Car-Free Households vs. Work Index",
       x = "Work Index", 
       y = "Car-Free Household Count") +
  theme_brand()

# Urban / Rural facet
ggplot(load_data, aes(x = work_log, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Work Index",
       subtitle = "by Urban/Rural",
       x = "Work Index", 
       y = "Car-Free Household Count") +
  theme_brand()

# --- Spearman’s correlation (1 line each) ---
overall_spearman <- cor.test(load_data$work_log, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$work_log[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$work_log[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman





#####################   Prop Homeowners   #####################

# --- Scatterplots ---
ggplot(load_data, aes(x = prop_homeowners, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  labs(title = "Car-Free Households vs. Proportion of Homeowners",
       x = "Proportion of Homeowners", 
       y = "Car-Free Household Count") +
  theme_brand()

ggplot(load_data, aes(x = prop_homeowners, y = car_free_count)) +
  geom_point(color = "#264653", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "#E76F51", fill = "#E76F51", alpha = 0.2) +
  facet_wrap(~ urban_rural) +
  labs(title = "Car-Free Households vs. Proportion of Homeowners",
       subtitle = "by Urban/Rural",
       x = "Proportion of Homeowners", 
       y = "Car-Free Household Count") +
  theme_brand()

# --- Spearman’s correlation (1 line each) ---
overall_spearman <- cor.test(load_data$prop_homeowners, load_data$car_free_count, method = "spearman"); overall_spearman
urban_spearman   <- cor.test(load_data$prop_homeowners[load_data$urban_rural == "urban"], load_data$car_free_count[load_data$urban_rural == "urban"], method = "spearman"); urban_spearman
rural_spearman   <- cor.test(load_data$prop_homeowners[load_data$urban_rural == "rural"], load_data$car_free_count[load_data$urban_rural == "rural"], method = "spearman"); rural_spearman












