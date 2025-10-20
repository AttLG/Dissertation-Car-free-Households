##############  Dissertation  ##############
# Date: 12/08/2025
# Desc: Model Diagnostics 

############################################
library(INLA)
library(tidyverse)
library(ggplot2)
library(purrr)

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

theme_set(theme_brand())


####################################################################################################
###                                 WAIC DIC comparison table                                    ##
####################################################################################################
model_comparison <- tibble::tibble(
  Model = c("Baseline (no spatial, no interaction)",
            "Spatial (BYM2, no interaction)",
            "Spatial + Interaction (BYM2)"),
  
  WAIC = c(
    fit_nb_baseline$waic$waic,
    fit_nb_bym2$waic$waic,
    fit_nb_interaction$waic$waic
  ),
  
  DIC = c(
    fit_nb_baseline$dic$dic,
    fit_nb_bym2$dic$dic,
    fit_nb_interaction$dic$dic
  )
)

print(model_comparison)

####################################################################################################
###                                        Q–Q plots                                            ##
####################################################################################################
# for
##   fit_nb_bym2          # full spatial model
##   fit_nb_interaction   # full spatial + urban_rural interactions

rqres_inla <- function(fit) {
  z <- qnorm(fit$cpo$pit)
  z[is.finite(z)]
}

rq_full   <- rqres_inla(fit_nb_bym2)
rq_inter  <- rqres_inla(fit_nb_interaction)

all_rq <- c(rq_full, rq_urban, rq_rural, rq_inter)
lims <- quantile(all_rq, probs = c(0.005, 0.995))  
xylims <- range(lims)

op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

qqnorm(rq_full,  main = "Q–Q: Full spatial (BYM2)",
       xlab = "Theoretical quantiles (N(0,1))",
       ylab = "Sample quantiles (rqres)",
       xlim = xylims, ylim = xylims, pch = 16, cex = 0.6)
abline(0, 1, col = "red", lwd = 2)

qqnorm(rq_inter, main = "Q–Q: Spatial + interaction",
       xlab = "Theoretical quantiles (N(0,1))",
       ylab = "Sample quantiles (rqres)",
       xlim = xylims, ylim = xylims, pch = 16, cex = 0.6)
abline(0, 1, col = "red", lwd = 2)

par(op)


####################################################################################################
###                                           PPC                                               ##
####################################################################################################
n_draws <- 500

eta_idx <- grep("^Predictor", rownames(inla.posterior.sample(1, fit_nb_interaction)[[1]]$latent))
stopifnot(length(eta_idx) == nrow(shp_data))

nb_size_index <- which(rownames(fit_nb_interaction$summary.hyperpar) == 
                         "size for the nbinomial observations (1/overdispersion)")

ppc_df <- purrr::map_dfr(seq_len(n_draws), function(d) {
  s <- inla.posterior.sample(1, fit_nb_interaction)[[1]]
  eta <- as.numeric(s$latent[eta_idx])
  mu <- exp(eta)
  size <- exp(as.numeric(s$hyperpar[nb_size_index]))
  y_rep <- rnbinom(length(eta_idx), size = size, mu = mu)
  tibble(draw = d, id = seq_len(length(eta_idx)), y_rep = y_rep)
})

gg_ppc_all <- ggplot() +
  geom_histogram(data = ppc_df, aes(x = y_rep, y = after_stat(density)),
                 bins = 40, fill = brand_cols["green_teal"], alpha = 0.3) +
  geom_histogram(data = observed, aes(x = y_obs, y = after_stat(density)),
                 bins = 40, colour = brand_cols["dark_teal"], fill = NA, linewidth = 0.7) +
  labs(title = "Posterior Predictive Check (Overall)",
       subtitle = "Simulated counts (fill) vs observed (outline)",
       x = "No-car households", y = "Density") +
  theme_brand()

print(gg_ppc_all)

