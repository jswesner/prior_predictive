
# Fit predator prey model -------------------------------------------------

library(tidyverse)
library(ggthemes)
library(rethinking)
library(cowplot)
library(here)
library(brms)
library(readr)


d_all <- read_csv("data/Portalier_etal_2018_Predator_Prey_Individual_Based_Data.csv") %>% 
  mutate(prey_g = MassPrey_kg*1000,
         pred_g = MassPredator_kg*1000,
         log_pred = log(pred_g),
         log_prey = log(prey_g))


d_all %>% group_by(CommonNamePredator) %>% 
  tally() %>% 
  filter(n>=5) %>% 
  arrange(n)

d_all %>% filter(CommonNamePredator == "Tonguefish") %>% 
  ggplot(aes(x = log_pred, y = log_prey)) + 
  geom_point()


d_tongue <- d_all %>% filter(CommonNamePredator == "Tonguefish") %>% 
  mutate(mean_log_pred = mean(log_pred),
         log_pred_c = log_pred - mean(log_pred))

# Fit models --------------------------------------------------------------

get_prior(log_prey ~ log_prey, data = d_tongue,
          family = gaussian())

weak_brm <- brm(log_prey ~ log_pred_c, data = d_tongue,
                family = gaussian(),
                prior = c(prior(normal(0, 1000), class = "Intercept"),
                          prior(normal(0, 1000), class = "b"),
                          prior(exponential(0.001), class = "sigma")))

saveRDS(weak_brm, file = "models/weak_brm.rds")

stronger_brm <- brm(log_prey ~ log_pred_c, data = d_tongue,
                    family = gaussian(),
                    prior = c(prior(normal(0, 10), class = "Intercept"),
                              prior(normal(0, 10), class = "b"),
                              prior(exponential(0.01), class = "sigma")))

saveRDS(stronger_brm, file = "models/stronger_brm.rds")

strongest_brm <- brm(log_prey ~ log_pred_c, data = d_tongue,
                     family = gaussian(),
                     prior = c(prior(normal(0, 1), class = "Intercept"),
                               prior(normal(0, 1), class = "b"),
                               prior(exponential(0.1), class = "sigma")))

saveRDS(strongest_brm, file = "models/strongest_brm.rds")

plot(conditional_effects(weak_brm), points = T)

plot(conditional_effects(stronger_brm), points = T)

plot(conditional_effects(strongest_brm), points = T)
