##############  Dissertation  ##############
# Date: 09/07/2024
# Desc: Investigate and choose distibution

############################################
library(tidyverse)
library(MASS)
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
saps_2022 <- read.csv(file = "data/data_for_checks.csv")

no_outliers <- saps_2022[saps_2022$nb_outlier == FALSE, ]

table(saps_2022$NTA_PTAL)



####################################################################################################
###                   Visualise the Distribution of the Response Variable                         ##
####################################################################################################
# Car-free Households 
ggplot(saps_2022, aes(x = car_free_count)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity", fill = "#2A9D8F", colour = "#264653") +
  labs(
    title = "Distribution of Car-Free Households",
    x = "Car-Free Household Count",
    y = "Frequency"
  ) +
  theme_brand()

summary(load_data$car_free_count)
var(load_data$car_free_count)

# --- Simple boxplot with outliers highlighted ---
ggplot(saps_2022, aes(x = "", y = car_free_count)) +
  geom_boxplot(
    fill = brand_cols["green_teal"],
    colour = brand_cols["dark_teal"],
    alpha = 0.7,
    outlier.colour = brand_cols["soft_red"],   # highlight outliers
    outlier.shape = 16,
    outlier.alpha = 0.6
  ) +
  labs(
    title = "Car-Free Household Count",
    y = "Car-Free Household Count",
    x = NULL
  ) +
  theme_brand() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


high_car_free <- load_data[load_data$car_free_count > 150, ]


####################################################################################################
###                                         Zero Inflated?                                        ##
####################################################################################################
# Fit a negative binomial (often better for overdispersed counts)
nb_model <- glm.nb(car_free_count ~ 1, data = saps_2022)

# Expected zero probability
expected_zero_prob <- dnbinom(0, size = nb_model$theta, mu = mean(load_data$car_free_count))
observed_zero_prop <- mean(load_data$car_free_count == 0)

observed_zero_prop
expected_zero_prob

####################################################################################################
###                                        Over Dispersed?                                        ##
####################################################################################################

saps_2022$HPIndex22_rel_scaled <- scale(saps_2022$HPIndex22_rel)
saps_2022$avg_household_size_scaled <- scale(saps_2022$avg_household_size)
saps_2022$urban_rural <- factor(saps_2022$urban_rural, levels = c("rural", "urban"))

##########################       Example  Poisson model        ##########################
test_model <- glm(car_free_count ~ prop_homeowners + 
             prop_age20_34 + 
             avg_household_size_scaled + 
             urban_rural,
           family = poisson(link = "log"), 
           data = saps_2022)


##########################      Pearson's Chisq Test        ##########################
# Extract values
y <- test_model$y                     # observed values
mu <- fitted(test_model)             # predicted means
resid_pearson <- residuals(test_model, type = "pearson")  # Pearson residuals

# Degrees of freedom
n <- length(y)                # number of observations
p <- length(coef(test_model))        # number of estimated parameters

# Pearson Chi-squared statistic
pearson_chi2 <- sum(resid_pearson^2)

# Dispersion statistic
dispersion <- pearson_chi2 / (n - p)

# Output
cat("Pearson Chi2:", pearson_chi2, "\n")
cat("Dispersion:", dispersion, "\n")


##########################      Residuals       ##########################

ggplot(data = data.frame(fitted = fitted(test_model), resid = residuals(test_model, type = "pearson")),
       aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Poisson Model: Pearson Residuals vs Fitted Values",
       x = "Fitted Values", y = "Pearson Residuals") +
  theme_brand()


##########################       Compare to NB        ##########################
mod_nb <- glm.nb(car_free_count ~ prop_homeowners + 
                   prop_age20_34 + 
                   avg_household_size_scaled + 
                   urban_rural,
                 data = model_data_df)

##########################       Chisq Test        ##########################
resid_pearson_nb <- residuals(mod_nb, type = "pearson")
pearson_chi2_nb <- sum(resid_pearson_nb^2)
dispersion_nb <- pearson_chi2_nb / df.residual(mod_nb)

cat("NB Pearson Chi2:", pearson_chi2_nb, "\n")
cat("NB Dispersion:", dispersion_nb, "\n")


##########################      Residuals side by side       ##########################
library(patchwork)

p1 <- ggplot(data.frame(
  fitted = fitted(test_model),
  resid = residuals(test_model, type = "pearson")),
  aes(x = fitted, y = resid)
) +
  geom_point(alpha = 0.3, color = "#264653") +  # Brand dark teal
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E9C46A") +  # Sand yellow
  labs(title = "Poisson", x = "Fitted Values", y = "Pearson Residuals") +
  theme_brand() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")  # Smaller title
  )

p2 <- ggplot(data.frame(
  fitted = fitted(mod_nb),
  resid = residuals(mod_nb, type = "pearson")),
  aes(x = fitted, y = resid)
) +
  geom_point(alpha = 0.3, color = "#264653") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E9C46A") +
  labs(title = "Negative Binomial", x = "Fitted Values", y = "Pearson Residuals") +
  theme_brand() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  )

# Combine and display
p1 + p2 +
  plot_annotation(
    title = "Pearson Residuals vs Fitted: Poisson vs Negative Binomial",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold")
    )
  )


##########################      QQ side by side       ##########################
# Extract Pearson residuals from your models
resid_poisson <- residuals(test_model, type = "pearson")
resid_nb <- residuals(mod_nb, type = "pearson")

# Plot side-by-side QQ plots
par(mfrow = c(1, 2))  # 1 row, 2 columns

# Poisson model
qqnorm(resid_poisson, main = "QQ Plot - Poisson Model")
qqline(resid_poisson, col = "red")

# Negative Binomial model
qqnorm(resid_nb, main = "QQ Plot - Negative Binomial Model")
qqline(resid_nb, col = "blue")

# Reset layout
par(mfrow = c(1, 1))


##########################      Predicted vs Actual      ##########################

# Add predicted values
saps_2022 <- saps_2022 %>%
  mutate(
    pred_poisson = predict(test_model, type = "response"),
    pred_nb      = predict(mod_nb, type = "response")
  )

# Reshape to long format for easier faceting
plot_df <- saps_2022 %>%
  dplyr::select(car_free_count, pred_poisson, pred_nb) %>%
  rename(Poisson = pred_poisson, NegBin = pred_nb) %>%
  pivot_longer(cols = c(Poisson, NegBin), names_to = "Model", values_to = "Predicted")

# Plot
ggplot(plot_df, aes(x = Predicted, y = car_free_count)) +
  geom_point(alpha = 0.4, colour = brand_cols["dark_teal"]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey30") +
  facet_wrap(~ Model) +
  labs(
    title = "Predicted vs Actual: Poisson vs Negative Binomial",
    x = "Predicted Car-Free Count",
    y = "Actual Car-Free Count"
  ) +
  theme_brand()



####################################################################################################
###    NB wuth offset                                                                     ##
####################################################################################################
mod_nb_offset <- glm.nb(car_free_count ~ prop_homeowners + 
                   prop_age20_34 + 
                   avg_household_size_scaled + 
                   urban_rural + 
                   offset(log(car_total_hh)),
                 data = model_data_df)


##########################       Chisq Test        ##########################
resid_pearson_nb <- residuals(mod_nb_offset, type = "pearson")
pearson_chi2_nb <- sum(resid_pearson_nb^2)
dispersion_nb <- pearson_chi2_nb / df.residual(mod_nb_offset)

cat("NB Pearson Chi2:", pearson_chi2_nb, "\n")
cat("NB Dispersion:", dispersion_nb, "\n")

## COMPARE RESIDUAL V FITTED 
# Residual plot for Negative Binomial (no offset)
p1 <- ggplot(data.frame(
  fitted = fitted(mod_nb),
  resid = residuals(mod_nb, type = "pearson")),
  aes(x = fitted, y = resid)
) +
  geom_point(alpha = 0.3, color = "#264653") +  # Dark teal
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E9C46A") +  # Sand yellow
  labs(title = "Neg. Binomial (No Offset)", x = "Fitted Values", y = "Pearson Residuals") +
  theme_brand() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  )

# Residual plot for Negative Binomial with offset
p2 <- ggplot(data.frame(
  fitted = fitted(mod_nb_offset),
  resid = residuals(mod_nb_offset, type = "pearson")),
  aes(x = fitted, y = resid)
) +
  geom_point(alpha = 0.3, color = "#264653") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E9C46A") +
  labs(title = "Neg. Binomial (Offset)", x = "Fitted Values", y = "Pearson Residuals") +
  theme_brand() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  )

# Combine and display
p1 + p2 +
  plot_annotation(
    title = "Pearson Residuals vs Fitted: NB vs NB (Offset)",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold")
    )
  )

####################################################################################################
###   Bayesian NB with offset                                                                    ##
####################################################################################################
library(INLA)

# Define formula WITHOUT any random effect
formula_nb_offset <- car_free_count ~ 
  prop_homeowners + 
  prop_age20_34 + 
  avg_household_size_scaled + 
  urban_rural

# Fit the model
mod_nb_inla <- inla(
  formula = formula_nb_offset,
  family = "nbinomial",
  data = model_data_df,
  offset = log(model_data_df$car_total_hh),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(compute = TRUE)
)

## COMPARE RESIDUAL V FITTED 
# ==== 1. MASS model (NB with offset) ====
p1 <- ggplot(data.frame(
  fitted = fitted(mod_nb_offset),
  resid = residuals(mod_nb_offset, type = "pearson")
), aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, color = "#264653") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E9C46A") +
  labs(title = "MASS NB", x = "Fitted Values", y = "Pearson Residuals") +
  theme_brand() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# ==== 2. INLA model (NB with offset, correct residuals) ====
# Extract fitted values
fitted_inla <- mod_nb_inla$summary.fitted.values$mean

# Extract theta (dispersion)
theta <- mod_nb_inla$summary.hyperpar[
  grep("size for the nbinomial", rownames(mod_nb_inla$summary.hyperpar)), "mean"
]

# Calculate variance and Pearson residuals
var_inla <- fitted_inla + (fitted_inla^2 / theta)
resid_inla <- (model_data_df$car_free_count - fitted_inla) / sqrt(var_inla)

# Plot
p2 <- ggplot(data.frame(
  fitted = fitted_inla,
  resid = resid_inla
), aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, color = "#264653") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E9C46A") +
  labs(title = "INLA NB", x = "Fitted Values", y = "Pearson Residuals") +
  theme_brand() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# ==== 3. Combine with proper main title ====
final_plot <- (p1 | p2) +
  plot_annotation(
    title = "Pearson Residuals vs Fitted: MASS vs INLA (NB with offset)",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold")
  ))

# ==== 4. Display ====
print(final_plot)














####################################################################################################
###    QQ Plots - count data                                                                     ##
####################################################################################################
my_counts <- saps_cleaned$car_free_count
my_trials <- saps_cleaned$car_total_hh


# Gaussian QQ plot
# Sort the sample data
sample_data <- sort(saps_cleaned$car_free_count)

# Generate theoretical quantiles from a normal distribution
theoretical_q_norm <- qnorm(ppoints(length(sample_data)),
                            mean = mean(sample_data, na.rm = TRUE),
                            sd = sd(sample_data, na.rm = TRUE))

# Q-Q Plot with consistent formatting
qqplot(theoretical_q_norm, sample_data,
       main = "Q-Q Plot: Normal Distribution",
       xlab = "Theoretical Quantiles (Normal)",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red", lwd = 2)



######          Poisson Dist          ######
# Estimate lambda (mean)
lambda_hat <- mean(my_counts)

# Generate theoretical Poisson quantiles
theoretical_q_pois <- qpois(ppoints(length(my_counts)), lambda = lambda_hat)

# Q-Q plot
qqplot(theoretical_q_pois, sort(my_counts),
       main = "Q-Q Plot: Poisson Distribution",
       xlab = "Theoretical Quantiles (Poisson)",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red", lwd = 2)



######          Binomial Dist          ######
# Estimate global probability of success
p_hat <- sum(my_counts) / sum(my_trials)

# Compute theoretical quantiles for each observation
theoretical_q_binom <- mapply(function(n, i) {
  qbinom(i, size = n, prob = p_hat)
},
n = my_trials,
i = ppoints(length(my_counts)))

# Q-Q plot
qqplot(theoretical_q_binom, sort(my_counts),
       main = "Q-Q Plot: Binomial with Varying Trials",
       xlab = "Theoretical Quantiles (Binomial)",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red", lwd = 2)



######          Neg Binomial Dist          ######
library(MASS)

# Fit a negative binomial distribution
nb_fit <- fitdistr(my_counts, "Negative Binomial")
size_hat <- nb_fit$estimate["size"]
mu_hat   <- nb_fit$estimate["mu"]

# Generate theoretical quantiles
theoretical_q_nb <- qnbinom(ppoints(length(my_counts)), size = size_hat, mu = mu_hat)

# Q-Q plot
qqplot(theoretical_q_nb, sort(my_counts),
       main = "Q-Q Plot: Negative Binomial Distribution",
       xlab = "Theoretical Quantiles (Neg. Binomial)",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red", lwd = 2)

######          Beta Binomial Dist          ######
# install.packages("VGAM")  # if not already installed
# library(VGAM)

######          Beta-Binomial Dist          ######

# # Load library
# library(VGAM)
# 
# # Prepare data: successes and trials
# # my_counts: number of successes
# # my_trials: number of trials per observation
# 
# # Estimate global alpha and beta parameters using method of moments
# p_hat <- sum(my_counts) / sum(my_trials)
# var_hat <- var(my_counts / my_trials)
# 
# # Method-of-moments estimates for alpha and beta
# common_n <- mean(my_trials)  # approximation; ideally trials are consistent
# tmp <- p_hat * (1 - p_hat) / var_hat - 1
# alpha_hat <- p_hat * tmp
# beta_hat  <- (1 - p_hat) * tmp
# 
# # Compute theoretical quantiles using rbetabinom.ab
# theoretical_q_bb <- mapply(function(n, i) {
#   qbetabinom.ab(i, size = n, shape1 = alpha_hat, shape2 = beta_hat)
# },
# n = my_trials,
# i = ppoints(length(my_counts)))
# 
# # Q-Q plot
# qqplot(theoretical_q_bb, sort(my_counts),
#        main = "Q-Q Plot: Beta-Binomial Distribution",
#        xlab = "Theoretical Quantiles (Beta-Binomial)",
#        ylab = "Sample Quantiles")
# abline(0, 1, col = "red", lwd = 2)



####################################################################################################
###    QQ Plots - prop data                                                                     ##
####################################################################################################

# Your observed data (replace with your actual data)
data <- model_data$car_own_prop  # Example data in (0,1)

# Calculate sample mean and variance
x_bar <- mean(data)
s2 <- var(data)

# Method of Moments estimation
v <- (x_bar * (1 - x_bar) / s2) - 1
alpha <- x_bar * v
beta <- (1 - x_bar) * v

# Print estimated parameters
cat("Estimated alpha:", alpha, "\nEstimated beta:", beta, "\n")

qqplot(qbeta(ppoints(length(data)), alpha, beta), 
       sort(data),
       main = "QQ Plot: Observed vs. Estimated Beta",
       xlab = "Theoretical Quantiles (Beta)",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red")




####################################################################################################
###    Check for Boundary Values                                                                  ##
####################################################################################################
summary(saps_2022$car_own_prop); any(saps_2022$car_own_prop <= 0 | saps_2022$car_own_prop >= 1)
# fail






####################################################################################################
###    Fit simplified models                                                                      ##
####################################################################################################
library(tidyverse)
library(MASS)

## Binomial
binom_model <- glm(cbind(car_none_count, total_car - car_none_count) ~ 1,
                   family = binomial,
                   data = saps_2022)
summary(binom_model)


## Negative Binomial
nb_model <- glm.nb(car_none_count ~ 1, data = saps_2022)
summary(nb_model)

## Quasi Binomial
quasibinom_model <- glm(cbind(car_none_count, total_car - car_none_count) ~ 1,
                        family = quasibinomial,
                        data = saps_2022)
summary(quasibinom_model)

# Beta Binomial 
# library(VGAM)
# 
# betabinom_model <- vglm(cbind(car_none_count, total_car - car_none_count) ~ 1,
#                         betabinomial,
#                         data = saps_2022)
# summary(betabinom_model)

# ZI Beta
library(betareg)

# Create proportion and adjust 0s or 1s if needed
saps_2022$car_own_prop_adj <- saps_2022$car_own_prop
saps_2022$car_own_prop_adj[saps_2022$car_own_prop_adj == 0] <- 0.001
saps_2022$car_own_prop_adj[saps_2022$car_own_prop_adj == 1] <- 0.999

beta_model <- betareg(car_own_prop_adj ~ 1, data = saps_2022)
summary(beta_model)






####################################################################################################
###    Compare simplified models                                                                      ##
####################################################################################################
summary(binom_model)
summary(nb_model)
summary(quasibinom_model)
summary(betabinom_model)
summary(beta_model)


AIC(binom_model)
AIC(nb_model)
AIC(quasibinom_model)

# check for overdispersion
# For binomial/quasibinomial models
dispersion_binom <- sum(residuals(binom_model, type = "pearson")^2) / df.residual(binom_model)
dispersion_quasi <- sum(residuals(quasibinom_model, type = "pearson")^2) / df.residual(quasibinom_model)
dispersion_nb <- sum(residuals(nb_model, type = "pearson")^2) / df.residual(nb_model)

dispersion_binom  # Expect ~1 for well-fitting binomial
# ~ 17
dispersion_quasi  # Should be ~17 (as seen)
dispersion_nb     # Should be ~1 if NB fits well
# ~ 1

# Deviance residuals for GLMs
plot(residuals(binom_model, type = "deviance"), main = "Binomial Deviance Residuals")
plot(residuals(quasibinom_model, type = "deviance"), main = "Quasibinomial Deviance Residuals")
plot(residuals(nb_model, type = "deviance"), main = "Negative Binomial Deviance Residuals")


# log likelihood
logLik(binom_model)
logLik(quasibinom_model)
logLik(nb_model)
logLik(betabinom_model)
logLik(beta_model)


# Compare fot on proportions scale
# Observed proportion
observed <- saps_2022$car_none_count / saps_2022$total_car

# Predicted values
pred_nb <- predict(nb_model, type = "response") / saps_2022$total_car  # convert to proportion
pred_bb <- predict(betabinom_model, type = "response")                 # already on proportion scale
pred_beta <- predict(beta_model, type = "response")                    # beta regression

# MAE
mae_nb <- mean(abs(observed - pred_nb))
mae_bb <- mean(abs(observed - pred_bb))
mae_beta <- mean(abs(observed - pred_beta))

# RMSE
rmse_nb <- sqrt(mean((observed - pred_nb)^2))
rmse_bb <- sqrt(mean((observed - pred_bb)^2))
rmse_beta <- sqrt(mean((observed - pred_beta)^2))

# View results
mae_nb; mae_bb; mae_beta
rmse_nb; rmse_bb; rmse_beta

## BB wins.







####################################################################################################
###    Fit different models to the data and compare                                               ##
####################################################################################################
####################################################################################################
###    Prep           `                                                                          ##
####################################################################################################
model_data <- saps_2022

write.csv(data.frame(Column_Names = colnames(saps_2022)), "output/column_names.csv", row.names = FALSE)

# 3 Categorical Variable
model_data$AREA_TYPE <- as.factor(model_data$AREA_TYPE)
levels(model_data$AREA_TYPE)
# set baselevel
model_data$AREA_TYPE <- relevel(model_data$AREA_TYPE, ref = "Rural")



exclude_vars <-  c("GUID",
                   "total_car",
                   "car_own_prop",
                   "car_own_prop_adj"
)

model_data <- saps_2022 %>%
  select(-(exclude_vars))







####################################################################################################
###    Fit Full Models           `                                                                ##
####################################################################################################
# neg binom
nb_full <- glm.nb(car_none_count ~ .,
                  data = model_data[, !(names(model_data) %in% c("GUID", "total_car", "car_own_prop", "car_own_prop_adj"))])

# beta binom
# Exclude only *true predictors* you don’t want
exclude_vars <- c("GUID", "car_own_prop", "car_own_prop_adj")
data_bb <- model_data[, !(names(model_data) %in% exclude_vars)]

# This works fine — total_car is used in response but not as predictor
betabinom_full <- vglm(cbind(car_none_count, total_car - car_none_count) ~ .,
                       family = betabinomial,
                       data = data_bb)

# beta reg
model_data$car_own_prop_adj <- (model_data$car_own_prop * (nrow(model_data) - 1) + 0.5) / nrow(model_data)

exclude_vars <- c("GUID", "total_car", "car_own_prop")

# Keep car_own_prop_adj in the data so it can be used as the response
data_beta <- model_data[, !(names(model_data) %in% exclude_vars)]

# Fit the model
beta_full <- betareg(car_own_prop_adj ~ ., data = data_beta)







####################################################################################################
###    Model Checks              `                                                                ##
####################################################################################################
# nb
plot(fitted(nb_full), residuals(nb_full, type = "deviance"),
     main = "NB: Residuals vs Fitted", xlab = "Fitted", ylab = "Deviance Residuals")
abline(h = 0, col = "red")


# bb
counts <- saps_2022$car_none_count
total  <- saps_2022$total_car
prop   <- saps_2022$car_own_prop_adj

resid_bb <- (counts / total) - predict(betabinom_full, type = "response")
plot(predict(betabinom_full, type = "response"), resid_bb,
     main = "BB: Raw Residuals vs Fitted", xlab = "Fitted", ylab = "Raw Residual")
abline(h = 0, col = "red")


# beta
plot(fitted(beta_full), residuals(beta_full, type = "quantile"),
     main = "Beta: Residuals vs Fitted", xlab = "Fitted", ylab = "Quantile Residuals")
abline(h = 0, col = "red")

### QQ plots
# NB
qqnorm(residuals(nb_full, type = "deviance"),
       main = "Q-Q Plot: Negative Binomial Residuals")
qqline(residuals(nb_full, type = "deviance"))


# Beta-Binomial (raw residuals)
qqnorm(residuals(betabinom_full),
       main = "Q-Q Plot: Beta-Binomial Residuals")
qqline(residuals(betabinom_full))


# Beta Regression (quantile residuals)
qqnorm(residuals(beta_full, type = "quantile"),
       main = "Q-Q Plot: Beta Regression Quantile Residuals")
qqline(residuals(beta_full, type = "quantile"))

#### log likelihood
logLik(nb_full)
logLik(betabinom_full)
logLik(beta_full)

# BB better for counts... makes no sense

#### predictor errors
library(Metrics)

# Predictions
pred_nb <- predict(nb_full, type = "response")
pred_bb <- predict(betabinom_full, type = "response")
pred_beta <- predict(beta_full, type = "response")

# True values (proportions)
y_true_prop <- saps_2022$car_none_count / saps_2022$total_car

# Calculate errors
mae_nb <- mae(y_true_prop, pred_nb / saps_2022$total_car)
rmse_nb <- rmse(y_true_prop, pred_nb / saps_2022$total_car)

mae_bb <- mae(y_true_prop, pred_bb)
rmse_bb <- rmse(y_true_prop, pred_bb)

mae_beta <- mae(y_true_prop, pred_beta)
rmse_beta <- rmse(y_true_prop, pred_beta)

mae_nb; mae_bb; mae_beta
rmse_nb; rmse_bb; rmse_beta

## again BB performs better... makes no sense!!

### Predictor significance
summary(nb_full)
summary(betabinom_full)
summary(beta_full)




####################################################################################################
###    Fit different models to the data and compare                                               ##
####################################################################################################
write.csv(data.frame(Column_Names = colnames(saps_2022)), "output/column_names.csv", row.names = FALSE)
exclude_vars <-  c("GUID",
                   "COUNTY",
                   "LA_NAME",
                   "bin"
)

model_data <- saps_2022 %>%
  select(-(exclude_vars))

###### Quasibinomial model
glm_quasi <- glm(car_own_prop ~ ., data = model_data, family = quasibinomial(link = "logit"))

###### Gaussian model with logit-transformed response
model_gaus <- model_data
model_gaus$logit_prop <- qlogis(pmin(pmax(model_data$car_own_prop, 1e-6), 1 - 1e-6))  # prevent Inf
model_gaus <- model_gaus[ , !(names(model_gaus) %in% "car_own_prop")]

lm_gaussian <- lm(logit_prop ~ ., data = model_gaus)

###### Beta regression (via betareg or brms).
model_beta <- model_data
# ZI adjustment
model_beta$car_own_prop_adj <- (model_beta$car_own_prop * (nrow(model_beta) - 1) + 0.5) / nrow(model_beta)
model_beta <- model_beta[ , !(names(model_beta) %in% "car_own_prop")]

lm_beta <- betareg(car_own_prop_adj ~ ., data = model_beta)


####################################################################################################
###    Model Summaries                                                                           ##
####################################################################################################
summary(glm_quasi)
summary(lm_gaussian)
summary(lm_beta)

####################################################################################################
###    Compare residuals                                                                          ##
####################################################################################################
par(mfrow = c(2,2))
plot(glm_quasi, which = 1:2)  # residuals vs fitted, QQ
plot(lm_gaussian, which = 1:2)
plot(lm_beta, which = 1:2)


# QQ plot for beta
resid_vec <- residuals(lm_beta, type = "deviance")  # replace with your model's residuals

# Create data frame for plotting
qq_data <- data.frame(
  sample = sort(resid_vec),
  theoretical = sort(qnorm(ppoints(length(resid_vec))))
)
# Q-Q plot
ggplot(qq_data, aes(x = theoretical, y = sample)) +
  geom_point(color = "darkblue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Q-Q Plot of Residuals",
    x = "Theoretical Quantiles (Normal)",
    y = "Sample Quantiles"
  ) +
  theme_minimal()















