## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)
library(ethnobotanyR)
library(ggplot2)
library(ggridges)
library(magrittr)
# The .bib is not rewritten at build time; regenerate it by hand if packages change

## ----cite_ethnobotanyR, comment=NA, echo=FALSE--------------------------------
citation("ethnobotanyR")

## ----data_head, echo=FALSE----------------------------------------------------
knitr::kable(head(homegardens[, 1:8]), caption = "First six rows of the homegardens data (first six use categories shown)")

## ----data_dims----------------------------------------------------------------
dim(homegardens)
nlevels(homegardens$informant)  # gardens
nlevels(homegardens$sp_name)    # species

## ----ethno_beta---------------------------------------------------------------
beta_hg <- ethno_beta(homegardens, level = 0.9)
# species grown in at least 50 gardens with the highest use probabilities
beta_hg[beta_hg$n >= 50, ] %>%
  arrange(desc(mean)) %>%
  head(8)

## ----ethno_beta_plot, fig.width=6, fig.height=3.5-----------------------------
coffee <- beta_hg[beta_hg$sp_name == "Coffea canephora" & beta_hg$k > 0, ]
ggplot(coffee, aes(x = mean, y = reorder(use, mean))) +
  geom_pointrange(aes(xmin = lower, xmax = upper)) +
  labs(x = "Share of gardens using Coffea canephora (90% interval)", y = "")

## ----ethno_boot_uses----------------------------------------------------------
avocado <- homegardens$sale[homegardens$sp_name == "Persea americana"]
bean <- homegardens$sale[homegardens$sp_name == "Phaseolus vulgaris"]

set.seed(1)
avocado_boot <- ethno_boot(avocado, statistic = mean, n1 = 1000)
bean_boot <- ethno_boot(bean, statistic = mean, n1 = 1000)

quantile(avocado_boot, c(0.05, 0.95))
quantile(bean_boot, c(0.05, 0.95))

## ----ethno_boot_diff----------------------------------------------------------
quantile(bean_boot - avocado_boot, c(0.05, 0.95))
mean(bean_boot > avocado_boot)  # probability beans are sold more often

## ----plot_boot_ridges, fig.width=7, fig.height=3.5----------------------------
boot_long <- data.frame(species = rep(c("Persea americana", "Phaseolus vulgaris"), each = 1000),
                        value = c(avocado_boot, bean_boot))
ggplot(boot_long, aes(x = value, y = species, fill = species)) +
  ggridges::geom_density_ridges() +
  ggridges::theme_ridges() +
  theme(legend.position = "none") +
  labs(y = "", x = "Bayesian bootstrap of the share of gardens selling the plant")

## ----ethno_boot_rare----------------------------------------------------------
no_use <- homegardens$hygiene[homegardens$sp_name == "Persea americana"]
sum(no_use)  # no garden reports this use
quantile(ethno_boot(no_use, mean), c(0.05, 0.95))
persea <- ethno_beta(homegardens[homegardens$sp_name == "Persea americana", ])
persea[persea$use == "hygiene", ]

## ----ethno_saturation, fig.width=5, fig.height=3.5----------------------------
sat <- ethno_saturation(homegardens, n_perm = 100)
ggplot(sat, aes(n_informants, mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line() +
  labs(x = "Gardens", y = "Distinct species-use pairs")

## ----ethno_consensus----------------------------------------------------------
common <- names(which(table(homegardens$sp_name) >= 30))
hg_common <- droplevels(homegardens[homegardens$sp_name %in% common, ])
cons <- ethno_consensus(hg_common)
cons$converged
summary(cons$competence$D)
head(cons$truth[order(-cons$truth$p_used), ], 8)

## ----competence_district, fig.width=6, fig.height=3.5-------------------------
comp <- merge(cons$competence, homegardens_info, by = "informant")
ggplot(comp, aes(district, D)) +
  geom_boxplot() +
  labs(x = "", y = "Estimated competence (D)")

## ----ethno_bayes_consensus----------------------------------------------------
coffee_data <- hg_common[hg_common$sp_name == "Coffea canephora", ]
coffee_D <- cons$competence$D[match(coffee_data$informant, cons$competence$informant)]
coffee_consensus <- ethno_bayes_consensus(coffee_data, answers = 2,
                                          prior_for_answers = coffee_D)
round(coffee_consensus["1", ], 3)  # probability each use is in the consensus

## ----homegardens_model--------------------------------------------------------
hg <- merge(merge(homegardens, homegardens_species, by = "sp_name"),
            homegardens_info, by = "informant")
hg <- droplevels(hg[hg$type != "Palm", ])  # one palm species: too few to estimate
fit <- glm(medicine ~ type + native + nearest_market_km, data = hg, family = binomial)
round(summary(fit)$coefficients, 2)

## ----homegardens_mixed, eval = requireNamespace("lme4", quietly = TRUE)-------
fit_mixed <- lme4::glmer(medicine ~ type + native + scale(nearest_market_km) + (1 | informant),
                         data = hg, family = binomial)
round(summary(fit_mixed)$coefficients, 2)

