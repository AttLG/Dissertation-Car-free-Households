##############  Dissertation  ##############
# Date: 10/08/2025
# Desc: Spatial Bayesian NB Model - output 

############################################
library(readxl)
library(dplyr)
library(tibble)
library(stringr)
library(knitr)


print(fit_nb_interaction$summary.fixed)

####################################################################################################
###                                No. Observ and FE parameters                                   ##
####################################################################################################
sample_size <- nrow(shp_data)
cat("Sample size (number of SAs):", sample_size, "\n")

num_fixed_effects <- nrow(fit_nb_interaction$summary.fixed)
cat("Number of parameters:", num_fixed_effects, "\n")




####################################################################################################
###                                       Hyperparameters                                         ##
####################################################################################################
# hyperparameters from model
hyp <- fit_nb_interaction$summary.hyperpar

# calc spatial variance and 95pc cri
spatial_var_mean <- 1 / hyp["Precision for area_index", "mean"]
spatial_var_sd <- NA  
spatial_var_low <- 1 / hyp["Precision for area_index", "0.975quant"] 
spatial_var_high <- 1 / hyp["Precision for area_index", "0.025quant"]

# convert to df 
hyperpar_tab <- data.frame(
  Variable = c(
    "Size (negative binomial overdispersion)",
    "Variance of spatial random effect",
    "Phi (BYM2 mixing parameter)"
  ),
  `Post. Mean` = c(
    hyp["size for the nbinomial observations (1/overdispersion)", "mean"],
    spatial_var_mean,
    hyp["Phi for area_index", "mean"]
  ),
  SD = c(
    hyp["size for the nbinomial observations (1/overdispersion)", "sd"],
    NA,  
    hyp["Phi for area_index", "sd"]
  ),
  `2.5% CrI` = c(
    hyp["size for the nbinomial observations (1/overdispersion)", "0.025quant"],
    spatial_var_low,
    hyp["Phi for area_index", "0.025quant"]
  ),
  `97.5% CrI` = c(
    hyp["size for the nbinomial observations (1/overdispersion)", "0.975quant"],
    spatial_var_high,
    hyp["Phi for area_index", "0.975quant"]
  ),
  check.names = FALSE
)

# round
hyperpar_tab <- hyperpar_tab %>%
  mutate(across(where(is.numeric), ~round(., 3)))


write.csv(hyperpar_tab, "model_hyperparameters.csv", row.names = FALSE)
print(hyperpar_tab)
print(fit_nb_interaction$summary.hyperpar)



####################################################################################################
###                                  Foxed Effects For Final Report                              ##
####################################################################################################
tab <- as.data.frame(fit_nb_interaction$summary.fixed[, c("mean","sd","0.025quant","0.975quant")])
tab$variable <- rownames(tab)

# rename
pretty_main <- function(x) {
  x <- str_replace(x, "\\(Intercept\\)", "Intercept")
  x <- str_replace(x, "urban_ruralrural", "Rural (vs Urban)")
  x <- str_replace(x, "dep_index_scaled", "Deprivation Index")
  x <- str_replace(x, "prop_age20_34", "Prop. Age 20-34")
  x <- str_replace(x, "prop_male_T1", "Prop. Male")
  x <- str_replace(x, "prop_bp_abroad", "Prop. Born Abroad")
  x <- str_replace(x, "prop_addr_change", "Prop. Recent Address Change")
  x <- str_replace(x, "avg_household_size_scaled", "Avg. Household Size")
  x <- str_replace(x, "prop_apt_flat", "Prop. Apartment/Flat")
  x <- str_replace(x, "prop_built_2016plus", "Prop. Built 2016+")
  x <- str_replace(x, "prop_no_heating", "Prop. No Central Heating")
  x <- str_replace(x, "prop_has_renewables", "Prop. Renewable Energy")
  x <- str_replace(x, "prop_volunteers", "Prop. Volunteers")
  x <- str_replace(x, "prop_managers_prof", "Prop. Managers/Professionals")
  x <- str_replace(x, "prop_agriculture", "Prop. Agriculture")
  x <- str_replace(x, "prop_long_commute", "Prop. Long Commute")
  x <- str_replace(x, "prop_disabled", "Prop. Disabled")
  x <- str_replace(x, "prop_carers", "Prop. Carers")
  x <- str_replace(x, "prop_good_health", "Prop. Good Health")
  x <- str_replace(x, "pop_density_scaled", "Population Density")
  x <- str_replace(x, "ptal_scoreLow Level of Service", "PTAL: Low Level of Service")
  x <- str_replace(x, "ptal_scoreMedium Level of Service", "PTAL: Medium Level of Service")
  x <- str_replace(x, "ptal_scoreMedium - High Level of Service", "PTAL: Med-High Level of Service")
  x <- str_replace(x, "ptal_scoreHigh Level of Service", "PTAL: High Level of Service")
  x <- str_replace(x, "log_food_shopping_scaled", "Log Food/Shopping Access")
  x <- str_replace(x, "log_social_leisure_scaled", "Log Social/Leisure Access")
  x <- str_replace(x, "log_work_scaled", "Log Work Access")
  x <- str_replace(x, "prop_lone_parent_families", "Prop. Lone Parent Families")
  x <- str_replace(x, "log_primary_edu_bin", "Log Primary Education")
  x <- str_replace(x, "log_secondary_edu_bin", "Log Secondary Education")
  x <- str_replace(x, "log_tertiary_edu_bin", "Log Tertiary Education")
  x
}

# apply
tab$Variable <- sapply(tab$variable, function(v) {
  if (str_detect(v, "urban_ruralrural:")) {
    pred <- str_remove(v, "urban_ruralrural:")
    pred_pretty <- pretty_main(pred)
    paste(pred_pretty, "x Rural")
  } else if (str_detect(v, ":urban_ruralrural")) {
    pred <- str_remove(v, ":urban_ruralrural")
    pred_pretty <- pretty_main(pred)
    paste(pred_pretty, "x Rural")
  } else {
    pretty_main(v)
  }
})

# order by main effects,  interactions
tab <- tab %>%
  mutate(
    Effect_Type = if_else(str_detect(variable, "urban_ruralrural"), "Interaction", "Main")
  ) %>%
  arrange(Effect_Type, Variable) %>%
  select(Variable, mean, sd, `0.025quant`, `0.975quant`)

# round
tab <- tab %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# order by themes
main_order <- c(
  "Intercept",
  "Rural (vs Urban)",

  "Deprivation Index ",
  "Prop. Age 20-34",
  "Prop. Male",
  "Prop. Born Abroad",
  "Prop. Recent Address Change",
  "Prop. Volunteers",
  "Prop. Disabled",
  "Prop. Carers",
  "Prop. Good Health",

  "Avg. Household Size ",
  "Prop. Apartment/Flat",
  "Prop. Built 2016+",
  "Prop. No Central Heating",
  "Prop. Renewable Energy",
  "Prop. Managers/Professionals",
  "Prop. Agriculture",
  "Prop. Long Commute",
  "Prop. Lone Parent Families",

  "Population Density ",
  "PTAL: Low Level of Service",
  "PTAL: Medium Level of Service",
  "PTAL: Med-High Level of Service",
  "PTAL: High Level of Service",
  "Log Food/Shopping Access ",
  "Log Social/Leisure Access ",
  "Log Work Access ",
  "Log Primary Education",
  "Log Secondary Education",
  "Log Tertiary Education"
)

# interactions order
interaction_order <- paste(main_order[-1], "x Rural") # skip Intercept for interaction

full_order <- c(main_order, interaction_order)
tab$Variable <- factor(tab$Variable, levels = full_order)
tab <- tab %>% arrange(Variable)

# table headers
names(tab) <- c("Variable", "Post. Mean", "SD", "2.5% CrI", "97.5% CrI")


write.csv(tab, "output/model_fixed_effects_v1.csv", row.names = FALSE)





####################################################################################################
###                                 IRR & % Change - Urban                                       ##
####################################################################################################
library(dplyr)
library(readxl)

# output
coefs <- as_tibble(fit_nb_interaction$summary.fixed, rownames = "Variable")

# increments & labels
var_info <- read_excel("data/Variable Increments and Labels.xlsx")

# apply increments
var_info <- var_info %>%
  mutate(
    increment_value = case_when(
      grepl("10 pp", Increment) ~ 0.10,
      grepl("1 SD", Increment) ~ 1,
      grepl("1 unit", Increment) ~ 1,
      grepl("per ref \\(No Service\\)", Increment) ~ 1,
      TRUE ~ NA_real_
    )
  )

# Urban effects only
main_df <- coefs %>%
  filter(!grepl(":", Variable)) %>%  
  filter(!Variable %in% c("(Intercept)", "urban_ruralrural")) %>% 
  left_join(var_info, by = c("Variable" = "Variable Code")) %>%
  mutate(
    Group = "Urban",
    β = mean,
    IRR = exp(β * increment_value),
    `% change` = (IRR - 1) * 100,
    # Get CrIs for β, then transform to % change
    IRR_lower = exp(`0.025quant` * increment_value),
    IRR_upper = exp(`0.975quant` * increment_value),
    `CrI lower` = (IRR_lower - 1) * 100,
    `CrI upper` = (IRR_upper - 1) * 100
  ) %>%
  select(
    Term = `Descriptive Label`,
    Group,
    β,
    Increment,
    IRR,
    `% change`,
    `% change lower`,
    `% change upper`,
    Variable
  )

print(main_df)
write.csv(main_df, "output/model_main_effects_pc_change.csv", row.names = FALSE)


####################################################################################################
###                                 IRR & % Change - Rural                                       ##
####################################################################################################

# output 
coefs <- as_tibble(fit_nb_interaction$summary.fixed, rownames = "Variable")

# increments/labels 
var_info <- read_excel("data/Variable Increments and Labels.xlsx")

# apply increments
var_info <- var_info %>%
  mutate(
    increment_value = case_when(
      grepl("10 pp", Increment) ~ 0.10,
      grepl("1 SD", Increment) ~ 1,
      grepl("1 unit", Increment) ~ 1,
      grepl("per ref \\(No Service\\)", Increment) ~ 1,
      TRUE ~ NA_real_
    )
  )

# urban effects 
urban_df <- coefs %>%
  filter(!grepl(":", Variable)) %>%
  filter(!Variable %in% c("(Intercept)", "urban_ruralrural")) %>%
  left_join(var_info, by = c("Variable" = "Variable Code")) %>%
  mutate(urban_β = mean) %>%
  select(Variable, urban_β)

#  interaction effects 
interaction_df <- coefs %>%
  filter(grepl(":", Variable)) %>%
  mutate(
    base_var = ifelse(grepl("^urban_ruralrural:", Variable),
                      sub("^urban_ruralrural:", "", Variable),
                      sub(":urban_ruralrural$", "", Variable))
  ) %>%
  left_join(var_info, by = c("Variable" = "Variable Code")) %>%
  left_join(urban_df, by = c("base_var" = "Variable")) %>%
  mutate(
    Group = "Rural",
    total_β = mean + urban_β,  # combined effect for rural
    IRR = exp(total_β * increment_value),
    `% change` = (IRR - 1) * 100,
    # Approx Cri for total (sum bounds of main + interaction)
    IRR_lower = exp((`0.025quant` + urban_β) * increment_value),
    IRR_upper = exp((`0.975quant` + urban_β) * increment_value),
    `CrI lower` = (IRR_lower - 1) * 100,
    `CrI upper` = (IRR_upper - 1) * 100
  ) %>%
  select(
    Term = `Descriptive Label`,
    Group,
    β = total_β,
    Increment,
    IRR,
    `% change`,
    `CrI lower`,
    `CrI upper`,
    Variable
  )

print(interaction_df)
write.csv(interaction_df, "output/model_rural_combined_effects_pc_change.csv", row.names = FALSE)



    
