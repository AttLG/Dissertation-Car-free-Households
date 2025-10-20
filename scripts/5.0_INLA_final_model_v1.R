##############  Dissertation  ##############
# Date: 05/08/2025
# Desc: Spatial Bayesian NB Model

############################################
library(readr)
library(tidyverse)
library(INLA)
library(sf)
# library(spdep)

rm(list = ls())

set.seed(20250810)
####################################################################################################
###                                      Import data                                              ##
####################################################################################################
load_data <- read.csv("data/data_for_checks.csv")

table(load_data$NTA_PTAL)

# st_layers("data/Small_Area_Boundaries_2022_subset.gpkg")
shape_data <- st_read("data/Small_Area_Boundaries_2022_subset.gpkg", layer = "Small_Area_Boundaries_2022_subset")

####################################################################################################
###                               Create Spatial Neighbours                                       ##
####################################################################################################
# Ensure geometry is valid
shape_data <- st_make_valid(shape_data)

# Create neighbors (Queen)
nb <- poly2nb(shape_data)

# Convert to INLA graph format
nb2INLA("adjacency.graph", nb)
nb_graph <- "adjacency.graph"

####################################################################################################
###                                       Join to SAPS Data                                       ##
####################################################################################################
load_data$area_id <- as.character(load_data$GUID)
shape_data$area_id <- as.character(shape_data$SA_GUID_2022) 

# Merge spatial data with model data
shp_data <- merge(shape_data, load_data, by = "area_id")
class(shp_data)

####################################################################################################
###                                       Prep Factors                                           ##
####################################################################################################

# Create INLA area index
shp_data$area_index <- as.numeric(as.factor(shp_data$area_id))

# # Convert to factor # rural base
shp_data$urban_rural <- factor(shp_data$urban_rural, levels = c("urban", "rural"))
table(shp_data$urban_rural)

# PTAL
shp_data$ptal_score <- factor(shp_data$NTA_PTAL,
                                 levels = c("No Service",
                                            "Low Level of Service",
                                            "Medium Level of Service",
                                            "Medium - High Level of Service",
                                            "High Level of Service"), 
                                 ordered = FALSE)
table(shp_data$ptal_score)

####################################################################################################
###                                    Scale Continuous                                     ##
####################################################################################################
# Scaling to mean = 0, SD = 1:
# comparable scale."effect per standard deviation".

shp_data$dep_index_scaled <- as.numeric(scale(shp_data$HPIndex22_rel))
shp_data$pop_density_scaled <- as.numeric(scale(shp_data$pop_density))
shp_data$avg_household_size_scaled <- as.numeric(scale(shp_data$avg_household_size))


####################################################################################################
###                            Log / Scale / Binary - Attractions Data                            ##
####################################################################################################

# Log transform all 
vars_to_log <- c(
  "work", "primary_edu", "secondary_edu", "tertiary_edu",
  "social_leisure", "food_shopping", "friends_family"
)

for (v in vars_to_log) {
  shp_data[[paste0("log_", v)]] <- log1p(shp_data[[v]])
}

# binary for edu 
edu_vars_log <- c("log_primary_edu", "log_secondary_edu", "log_tertiary_edu")

for (v in edu_vars_log) {
  shp_data[[paste0(v, "_bin")]] <- as.integer(shp_data[[v]] > 0)
}

# scale others
nonedu_to_scale <- c("work", "social_leisure", "food_shopping", "friends_family")

for (v in nonedu_to_scale) {
  log_name    <- paste0("log_", v)
  scaled_name <- paste0(log_name, "_scaled")
  shp_data[[scaled_name]] <- as.numeric(scale(shp_data[[log_name]]))
}

####################################################################################################
###                                          Log Offset Term                                     ##
####################################################################################################

shp_data$log_total_car <- log(shp_data$car_total_hh)

####################################################################################################
###                                         Predictors                                           ##
####################################################################################################
predictors <- c(
  "dep_index_scaled",           
  "prop_age20_34",
  "prop_male_T1",
  "prop_bp_abroad",
  "prop_addr_change",
  "avg_household_size_scaled",     
  "prop_apt_flat",
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
  "prop_lone_parent_families",
  "pop_density_scaled",            
  "urban_rural",
  "ptal_score",
  "log_food_shopping_scaled",      
  "log_social_leisure_scaled",     
  "log_work_scaled", 
  "log_primary_edu_bin",           
  "log_secondary_edu_bin",         
  "log_tertiary_edu_bin"           
)


####################################################################################################
###                                          Prep Model                                        ##
####################################################################################################
set.seed(20250810)
options(contrasts = c("contr.treatment", "contr.poly"))
contrasts(shp_data$ptal_score) <- contr.treatment(levels(shp_data$ptal_score))

## formula 
stopifnot(exists("predictors"))
form_base <- reformulate(termlabels = predictors, response = "car_free_count")

# priors
# fixed effects: N(0,1) 
control.fixed <- list(
  mean.intercept = 0, prec.intercept = 1e-4,
  mean = 0, prec = 1
)

# dispersion prior
control.family.nb <- list(
  hyper = list(
    theta = list(prior = "pc.prec", param = c(1, 0.25))
  )
)

####################################################################################################
###                                     Fit Baseline Model                                       ##
####################################################################################################
# Formula WITHOUT spatial or interaction
form_baseline <- as.formula(
  paste(
    "car_free_count ~",
    paste(predictors, collapse = " + ")
  )
)

# Fit baseline model
fit_nb_baseline <- inla(
  formula = form_baseline,
  data = shp_data,
  family = "nbinomial",
  offset = log_total_car,
  control.fixed = control.fixed,           
  control.family = control.family.nb,    
  control.compute = list(waic = TRUE, dic = TRUE, cpo = TRUE, config = TRUE),
  control.predictor = list(compute = TRUE, link = 1),
  verbose = FALSE
)

####################################################################################################
###                                       Fit Spatial Model                                       ##
####################################################################################################

# formula with spatial term
form_spatial <- as.formula(
  paste(
    "car_free_count ~",
    paste(predictors, collapse = " + "),
    "+ f(area_index, model = 'bym2', graph = nb_graph, scale.model = TRUE,
         hyper = list(
           prec = list(prior = 'pc.prec', param = c(1, 0.01)),
           phi  = list(prior = 'pc', param = c(0.5, 2/3))
         ))"
  )
)

# Fit spatial model
# same priors as baseline
# same dispersion prior as baseline
fit_nb_bym2 <- inla(
  formula = form_spatial,
  data = shp_data,
  family = "nbinomial",
  offset = log_total_car,
  control.fixed = control.fixed,
  control.family = control.family.nb,      
  control.compute = list(waic = TRUE, dic = TRUE, cpo = TRUE, config = TRUE),
  control.predictor = list(compute = TRUE, link = 1),
  verbose = FALSE
)

####################################################################################################
###                                       Results                                                ##
####################################################################################################
cat("\n===== Fixed effects (post. mean, sd, 95% CrI) =====\n")
print(fit_nb_bym2$summary.fixed[, c("mean","sd","0.025quant","0.975quant")])



####################################################################################################
###                      Fit Model with Interactions for Urban / Rural                            ##
####################################################################################################
# interactions for all predictors
predictors_inter <- paste0(predictors_ur, " * urban_rural")

form_spatial_interaction <- as.formula(
  paste(
    "car_free_count ~",
    paste(predictors_inter, collapse = " + "),
    "+ f(area_index, model = 'bym2', graph = nb_graph, scale.model = TRUE,
         hyper = list(
           prec = list(prior = 'pc.prec', param = c(1, 0.01)),
           phi  = list(prior = 'pc', param = c(0.5, 2/3))
         ))"
  )
)

# Fit finl model
fit_nb_interaction <- inla(
  formula = form_spatial_interaction,
  data = shp_data,
  family = "nbinomial",
  offset = log_total_car,
  control.fixed = control.fixed,           # same priors as baseline
  control.family = control.family.nb,      # same dispersion prior as baseline
  control.compute = list(waic = TRUE, dic = TRUE, cpo = TRUE, config = TRUE),
  control.predictor = list(compute = TRUE, link = 1),
  verbose = FALSE
)

####################################################################################################
###                                       Results                                                ##
####################################################################################################
cat("\n===== Fixed effects (post. mean, sd, 95% CrI) =====\n")
print(fit_nb_interaction$summary.fixed[, c("mean","sd","0.025quant","0.975quant")])

print(fit_nb_interaction$summary.hyperpar)
    

  

