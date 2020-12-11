library(tidyverse)
library(ggthemes)
library(rethinking)
library(cowplot)
library(here)
library(janitor)
library(brms)


# Fit predator prey model  -------------------------------------------------


d <- readRDS(here("data/pred-prey-mass.RDS"))

d$log_pred <- log(d$pred)
d$log_prey <- log(d$prey)

weak_brm_d <- brm(log_prey ~ log_pred, data = d,
                family = gaussian(),
                prior = c(prior(normal(0, 1000), class = "Intercept"),
                          prior(normal(0, 1000), class = "b"),
                          prior(exponential(0.0001), class = "sigma")))

saveRDS(weak_brm_d, file = here("models/weak_brm_d.rds"))

stronger_brm_d <- brm(log_prey ~ log_pred, data = d,
                    family = gaussian(),
                    prior = c(prior(normal(0, 10), class = "Intercept"),
                              prior(normal(0, 10), class = "b"),
                              prior(exponential(0.01), class = "sigma")))

saveRDS(stronger_brm_d, file = here("models/stronger_brm_d.rds"))

strongest_brm_d <- brm(log_prey ~ log_pred, data = d,
                     family = gaussian(),
                     prior = c(prior(normal(0, 1), class = "Intercept"),
                               prior(normal(0, 1), class = "b"),
                               prior(exponential(.5), class = "sigma")))

saveRDS(strongest_brm_d, file = here("models/strongest_brm_d.rds"))

posts_weak <- posterior_samples(weak_brm_d) %>% as_tibble() %>% clean_names() %>% mutate(priors = "weak")
posts_stronger <- posterior_samples(stronger_brm_d) %>% as_tibble() %>% clean_names() %>% mutate(priors = "stronger")
posts_strongest <- posterior_samples(strongest_brm_d) %>% as_tibble() %>% clean_names() %>% mutate(priors = "strongest")



# check simulation consistency - re-run all code below a bunch of times
d <- readRDS(here("data/pred-prey-mass.RDS"))

d$log_pred <- log(d$pred)
d$log_prey <- log(d$prey)

a <- readRDS(here("models/weak_brm_d.rds"))
b <- readRDS(here("models/stronger_brm_d.rds"))
c <- readRDS(here("models/strongest_brm_d.rds"))

a_sim_error <- update(a, newdata = d)
b_sim_error <- update(b, newdata = d)
c_sim_error <- update(c, newdata = d)

# predictions for an average-sized predator (well, median-sized)

a_posts <- posterior_samples(a_sim_error) %>% as_tibble() %>% 
  clean_names() %>% mutate(iter = 1:nrow(.), 
                           y_sims = b_intercept + 
                             b_log_pred*median(d$log_pred) + rnorm(nrow(.), 0, sigma),
                           mod = "weak")

b_posts <- posterior_samples(b_sim_error) %>% as_tibble() %>% 
  clean_names() %>% mutate(iter = 1:nrow(.), 
                           y_sims = b_intercept + 
                             b_log_pred*median(d$log_pred) + rnorm(nrow(.), 0, sigma),
                           mod = "stronger")

c_posts <- posterior_samples(c_sim_error) %>% as_tibble() %>% 
  clean_names() %>% mutate(iter = 1:nrow(.), 
                           y_sims = b_intercept + 
                             b_log_pred*median(d$log_pred) + rnorm(nrow(.), 0, sigma),
                           mod = "strongest")

bind_rows(a_posts, b_posts, c_posts) %>% 
  group_by(mod) %>% 
  summarize(median = median(y_sims),
            mean = mean(y_sims),
            sd = sd(y_sims),
            lower = quantile(y_sims, probs = 0.025),
            upper = quantile(y_sims, probs = 0.975))



# Repeat with a centered dataset -------------------------------------------
# check simulation consistency - re-run all code below a bunch of times
d <- readRDS(here("data/pred-prey-mass.RDS"))

d$log_pred <- log(d$pred)
d$log_prey <- log(d$prey)

set.seed(2845476)
d_cent <- d %>% mutate(log_pred = log_pred - mean(log_pred))
plot(d_cent$log_pred, d_cent$log_prey)

a <- readRDS(here("models/weak_brm_d.rds"))
b <- readRDS(here("models/stronger_brm_d.rds"))
c <- readRDS(here("models/strongest_brm_d.rds"))

a_sim_small <- update(a, newdata = d_cent)
b_sim_small <- update(b, newdata = d_cent)
c_sim_small <- update(c, newdata = d_cent)



# predictions for an average-sized predator (well, median-sized)
a_posts <- posterior_samples(a_sim_small) %>% as_tibble() %>% 
  clean_names() %>% mutate(iter = 1:nrow(.), 
                           y_sims = b_intercept + rnorm(nrow(.), 0, sigma),
                           mod = "weak")

b_posts <- posterior_samples(b_sim_small) %>% as_tibble() %>% 
  clean_names() %>% mutate(iter = 1:nrow(.), 
                           y_sims = b_intercept + rnorm(nrow(.), 0, sigma),
                           mod = "stronger")

c_posts <- posterior_samples(c_sim_small) %>% as_tibble() %>% 
  clean_names() %>% mutate(iter = 1:nrow(.), 
                           y_sims = b_intercept + rnorm(nrow(.), 0, sigma),
                           mod = "strongest")

(summary_small <- bind_rows(a_posts, b_posts, c_posts) %>% 
  group_by(mod) %>% 
  summarize(median = median(y_sims),
            mean = mean(y_sims),
            sd = sd(y_sims),
            lower = quantile(y_sims, probs = 0.025),
            upper = quantile(y_sims, probs = 0.975)))

# d_cent %>% 
#   ggplot(aes(x = log_pred, y = log_prey)) +
#   geom_point() +
#   geom_point(data = summary_small, aes(x = 0, y = median,
#                                        color = mod), shape = 9, size = 2)

ggplot() +
  geom_point(data = summary_small, aes(x = 0, y = median,
                                       color = mod), shape = 9, size = 2) +
  geom_point(data = d_cent %>% filter(log_pred >=-0.05 & log_pred <= 0.05), 
             aes(x = log_pred, y = log_prey))



