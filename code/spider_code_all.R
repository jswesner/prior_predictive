library(tidyverse)
library(ggthemes)
library(rethinking)
library(cowplot)
library(lubridate)
library(brms)
library(janitor)
library(scales)
library(here)

# load spider data from Warmbold and Wesner 2018 
spiders <- read.csv(here("data/spiders.csv"))



# show parameters that need priors  
get_prior(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"))


# Simulate from prior predictive distribution --------------------------------------------------------------

# Fit with brms package - specify to sample only priors via sample_prior = "only"
spiders_wide <- brm(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"),
                   prior = c(prior(normal(0, 10), class = "b"),
                              prior(normal(0, 10), class = "Intercept"),
                              prior(exponential(0.1), class = "sd")),
                   chains = 1, iter = 1000, sample_prior = "only")

spiders_narrow <- brm(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"),
                    prior = c(prior(normal(0, 1), class = "b"),
                              prior(normal(0, 1), class = "Intercept"),
                              prior(exponential(1), class = "sd")),
                    chains = 1, iter = 1000, sample_prior = "only")

spiders_narrowest <- brm(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"),
                    prior = c(prior(normal(0, 0.1), class = "b"),
                              prior(normal(0, 0.1), class = "Intercept"),
                              prior(exponential(2), class = "sd")),
                    chains = 1, iter = 1000, sample_prior = "only")

saveRDS(spiders_wide, file = here("models/spiders_wide.rds"))
saveRDS(spiders_narrow, file = here("models/spiders_narrow.rds"))
saveRDS(spiders_narrowest, file = here("models/spiders_narrowest.rds"))


# Fit the model to data ---------------------------------------------------

# This step is done here only to facilitate making the plots in the main text. Otherwise, this step would wait
# until after choosing priors.
spiders_narrow_post <- brm(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"),
                      prior = c(prior(normal(0, 1), class = "b"),
                                prior(normal(0, 1), class = "Intercept"),
                                prior(exponential(1), class = "sd")),
                      chains = 1, iter = 1000)

# spiders_narrow_post <- readRDS(here("models/spiders_narrow_post.rds"))

spider_post <- fitted(spiders_narrow_post, summary = F, scale = "response") %>% t() %>% as_tibble() %>% 
  mutate(trt= spiders$trt,
         cage = spiders$cage,
         date = spiders$date) %>% 
  pivot_longer(cols = c(-trt, -cage, -date)) %>% 
  clean_names() %>% 
  mutate(prior_post = "posterior",
         prior_type = "narrow",
         Treatment = trt)




# Make Plots --------------------------------------------------------------

# Extract conditional means from each model
preds_wide <- fitted(spiders_wide, summary = F, scale = "response") %>% t() %>% as_tibble() %>% 
  mutate(trt = spiders$trt,
         cage = spiders$cage,
         date = spiders$date) %>% 
  pivot_longer(cols = c(-trt, -cage, -date)) %>% 
  clean_names() %>% 
  mutate(prior_post = "prior",
         prior_type = "wide")


preds_narrow <- fitted(spiders_narrow, summary = F, scale = "response") %>% t() %>% as_tibble() %>% 
  mutate(trt = spiders$trt,
         cage = spiders$cage,
         date = spiders$date) %>% 
  pivot_longer(cols = c(-trt, -cage, -date)) %>% 
  clean_names() %>% 
  mutate(prior_post = "prior",
         prior_type = "narrow")


preds_narrowest <- fitted(spiders_narrowest, summary = F, scale = "response") %>% t() %>% as_tibble() %>% 
  mutate(trt = spiders$trt,
         cage = spiders$cage,
         date = spiders$date) %>% 
  pivot_longer(cols = c(-trt, -cage, -date)) %>% 
  clean_names() %>% 
  mutate(prior_post = "prior",
         prior_type = "narrowest")

# combine them
preds_all <- bind_rows(preds_wide, preds_narrow, preds_narrowest) %>% 
  mutate(facet = case_when(prior_type == "wide" ~ "a",
                           prior_type == "narrow" ~ "b",
                           prior_type == "narrowest" ~ "c"),
         date = as_date(date)) %>% 
  rename(Treatment = trt)


# Plot conditional means
spiders_top_a <- preds_all %>% filter(facet == "a") %>% 
  ggplot(aes(x = date, y = value, fill = Treatment, group = interaction(Treatment, date))) +
  geom_violin(position = position_dodge(width = 5)) +
  scale_y_log10(labels = c("0.00001", "0.0001","0.001","0.01", "1","10", "100", "1000", "100,000", "100,000,000"), 
                breaks = c(0.00001, 0.0001, 0.001, 0.01, 1, 10, 100, 1000, 100000, 100000000)) +
  scale_fill_grey() +
  # facet_grid(.~facet) +
  coord_cartesian(ylim = c(0.00001, 100000000)) + 
  theme_classic() +
  labs(y = "Mean Number of Spiders",
       x = "Date",
       title = expression(paste("Simulated Mean (",lambda["i"], " = ", alpha + beta[1]*x["ijt"],"...))")),
       subtitle = "Weak Priors") +
  theme(legend.position = "top",
        text = element_text(size = 16),
        legend.title = element_blank()) 
  

spiders_top_b <- preds_all %>% filter(facet == "b") %>% 
  ggplot(aes(x = date, y = value, fill = Treatment, group = interaction(Treatment, date))) +
  geom_violin(position = position_dodge(width = 5)) +
  scale_y_log10(labels = c("0.00001", "0.0001","0.001","0.01", "1","10", "100", "1000", "100,000", "100,000,000"), 
                breaks = c(0.00001, 0.0001, 0.001, 0.01, 1, 10, 100, 1000, 100000, 100000000)) +
  scale_fill_grey() +
  # facet_grid(.~facet) +
  coord_cartesian(ylim = c(0.00001, 100000000)) + 
  theme_classic() +
  labs(y = "Mean Number of Spiders",
       x = "Date",
       title = "",
       subtitle = "Stronger Priors (with Posterior)") +
  theme(legend.position = "top",
        text = element_text(size = 16),
        legend.title = element_blank()) +
  geom_violin(data = spider_post, fill = "#E69F00",position = position_dodge(width = 5)) # add posterior


spiders_top_c <- preds_all %>% filter(facet == "c") %>% 
  ggplot(aes(x = date, y = value, fill = Treatment, group = interaction(Treatment, date))) +
  geom_violin(position = position_dodge(width = 5)) +
  scale_y_log10(labels = c("0.00001", "0.0001","0.001","0.01", "1","10", "100", "1000", "100,000", "100,000,000"), 
                breaks = c(0.00001, 0.0001, 0.001, 0.01, 1, 10, 100, 1000, 100000, 100000000)) +
  scale_fill_grey() +
  # facet_grid(.~facet) +
  coord_cartesian(ylim = c(0.00001, 100000000)) + 
  theme_classic() +
  labs(y = "Mean Number of Spiders",
       x = "Date",
       title = "",
       subtitle = "Strongest Priors") +
  theme(legend.position = "top",
        text = element_text(size = 16),
        legend.title = element_blank())




# Simulate number of spiders in a new cage --------------------------------

# data to simulate across
newdata <- data.frame(datefac = as.factor(max(spiders$date)),
                      trt = unique(spiders$trt),
                      cage = "new")

# Simulate data sets with the predict() function from brms
predsint_wide <- predict(spiders_wide, newdata = newdata, summary = F, allow_new_levels = T) %>% as_tibble() %>% mutate(sims = 1:nrow(.), facet = "c") 
predsint_narrow <- predict(spiders_narrow, newdata = newdata, summary = F, allow_new_levels = T) %>% as_tibble() %>% mutate(sims = 1:nrow(.), facet = "d")
predsint_narrowest <- predict(spiders_narrowest, newdata = newdata, summary = F, allow_new_levels = T) %>% as_tibble() %>% mutate(sims = 1:nrow(.), facet = "e")
predsint_all = bind_rows(predsint_wide, predsint_narrow, predsint_narrowest) %>% 
  pivot_longer(cols = c(V1, V2, V3)) %>% 
  mutate(trt = case_when(name == "V1" ~ "buffalo",
                         name == "V2" ~ "fishless",
                         TRUE ~ "green"))

# do the same for the posterior
spider_pred_post <- predict(spiders_narrow_post, newdata = newdata, 
                            summary = F, allow_new_levels = T) %>% 
  as_tibble() %>% mutate(sims = 1:nrow(.)) %>% 
  pivot_longer(cols = c(V1, V2, V3)) %>% 
  mutate(trt = case_when(name == "V1" ~ "buffalo",
                         name == "V2" ~ "fishless",
                         TRUE ~ "green"),
         prior_post = "Posterior")




# Make plots
spiders_bottom_a <- predsint_all %>% filter(facet == "c") %>% 
  ggplot(aes(x = trt, y = value)) + 
  geom_point(position = position_jitter(width = 0.1), shape = 21) +
  scale_y_log10(labels = c("1", "10", "100", "1,000", "100,000", "100,000,000"), breaks = c(1, 10, 100, 1000, 100000, 100000000)) +
  # facet_grid(.~facet, scales = "free_y") +
  theme_classic() +
  labs(y = "Number of Spiders", 
       x = "Treatment",
       title = expression(paste("Simulated Data (",y["i"]^{"new"}, " ~ Poisson(", lambda["i"],"))")),
       subtitle = "Weak Priors") +
  theme(text = element_text(size = 16),
        plot.title = element_text(size = 13.5))
  

spiders_bottom_b <- predsint_all %>% filter(facet == "d") %>% 
  mutate(prior_post = "Prior") %>% 
  bind_rows(spider_pred_post) %>% # combine with posterior predictions
  mutate(prior_post = fct_relevel(prior_post, "Prior")) %>% 
  ggplot(aes(x = trt, y = value, color = prior_post)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.4), shape = 21) +
  scale_y_log10(labels = c("1", "10", "100", "1,000", "100,000", "100,000,000"), breaks = c(1, 10, 100, 1000, 100000, 100000000)) +
  # facet_grid(.~facet, scales = "free_y") +
  scale_color_colorblind() +
  theme_classic() +
  labs(y = "Number of Spiders", 
       x = "Treatment",
       title = "",
       subtitle = "Stronger Priors (with Posterior)") +
  # geom_point(data = spider_pred_post, position = position_jitter(width = 0.1), shape = 21,
  #            color = "#E69F00") +
  theme(text = element_text(size = 16)) +
  guides(color = F) +
  NULL

spiders_bottom_c <- predsint_all %>% filter(facet == "e") %>%  
  ggplot(aes(x = trt, y = value)) + 
  geom_point(position = position_jitter(width = 0.1), shape = 21) +
  scale_y_log10(labels = c("1", "10", "100", "1,000", "100,000", "100,000,000"), breaks = c(1, 10, 100, 1000, 100000, 100000000)) +
  # facet_grid(.~facet, scales = "free_y") +
  theme_classic() +
  labs(y = "Number of Spiders", 
       x = "Treatment",
       title = "",
       subtitle = "Strongest Priors") +
  theme(text = element_text(size = 16))


# combine plots
spiders_priors <- plot_grid(spiders_top_a,
                            spiders_top_b,
                            spiders_top_c,
                            spiders_bottom_a,
                            spiders_bottom_b,
                            spiders_bottom_c, ncol = 3, align = "v")

saveRDS(spiders_priors, file = here("plots/spiders_priors.rds"))
ggsave(spiders_priors, file = here("plots/spiders_priors.tiff"), dpi = 400, width = 16, height = 11)
ggsave(spiders_priors, file = here("plots/spiders_priors.jpg"), dpi = 400, width = 16, height = 11)


