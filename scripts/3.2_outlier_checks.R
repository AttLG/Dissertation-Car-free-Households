##############  Dissertation  ##############
# Date: 31/07/2025
# Desc: Outlier Checks

############################################
library(readr)
library(tidyverse)
library(ggplot2)
library(MASS)


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

# apply 
theme_set(theme_brand())

####################################################################################################
###                                      Import data                                              ##
####################################################################################################
load_data <- read.csv(file = "data/data_for_checks.csv")


####################################################################################################
###                                      NB Outliers                                              ##
####################################################################################################


# fir NB model 
nb_model <- glm.nb(car_free_count ~ 1, data = load_data)

# mean and dispersion 
mu_hat <- predict(nb_model, type = "response")
theta <- nb_model$theta  # estimated dispersion parameter

# 3. Calculate upper tail probability for each observation
# i.e. P(X > observed value) under NB
car_free_probs <- pnbinom(load_data$car_free_count, size = theta, mu = mu_hat, lower.tail = FALSE)

# 4. Flag observations as outliers if tail probability is very low (e.g., < 0.01)
load_data$nb_outlier <- car_free_probs < 0.01

# 5. Summary
table(load_data$nb_outlier)

#### View outliers
car_free_outliers <-load_data %>% filter(nb_outlier == TRUE)

write.csv(car_free_outliers, file = "output/outliers_car_free_count.csv", row.names = FALSE)

summary(car_free_outliers$car_total_hh)
summary(car_free_outliers$car_free_count)


summary(car_free_outliers$HPIndex22_rel)
table(car_free_outliers$AREA_TYPE)

table(car_free_outliers$NTA_PTAL)


















