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


# Fit Models - Posterior --------------------------------------------------

spiders_wide_post <- brm(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"),
                      prior = c(prior(normal(0, 10), class = "b"),
                                prior(normal(0, 10), class = "Intercept"),
                                prior(exponential(0.1), class = "sd")),
                      chains = 1, iter = 1000)

spiders_narrow_post <- brm(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"),
                      prior = c(prior(normal(0, 1), class = "b"),
                                prior(normal(0, 1), class = "Intercept"),
                                prior(exponential(1), class = "sd")),
                      chains = 1, iter = 1000)

spiders_narrowest_post <- brm(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"),
                           prior = c(prior(normal(0, 0.1), class = "b"),
                                     prior(normal(0, 0.1), class = "Intercept"),
                                     prior(exponential(2), class = "sd")),
                           chains = 1, iter = 1000)

test <- conditional_effects(spiders_wide_post)
test2 <- conditional_effects(spiders_narrow_post)
test3 <- conditional_effects(spiders_narrowest_post)

wide_cond <- test$`trt:datefac` %>% as_tibble() %>% clean_names() %>% mutate(Priors = "weak")
narrow_cond <- test2$`trt:datefac` %>% as_tibble() %>% clean_names() %>% mutate(Priors = "stronger")
narrowest_cond <- test3$`trt:datefac` %>% as_tibble() %>% clean_names() %>% mutate(Priors = "strongest")

both_cond <- bind_rows(wide_cond, narrow_cond, narrowest_cond)


spider_supplementary <- both_cond %>%
  mutate(webs = estimate) %>%
  mutate(Priors = fct_relevel(Priors, "weak")) %>%
  ggplot(aes(x = reorder(interaction(datefac,trt), webs), y = webs)) +
  geom_pointrange(aes(ymin = lower, ymax = upper, color = Priors), position = position_dodge(width = 0.3)) +
  geom_point(data = spiders, aes( y = webs), alpha = 0.3,
             position = position_jitter(height = 0.1, width = 0)) +
  theme_classic() +
  labs(x = "Date x Treatment ranked by # webs",
       y = "Number of spiders webs") +
  scale_color_colorblind() +
  coord_flip()

saveRDS(spider_supplementary, file = here("plots/spider_supplementary.rds"))
ggsave(spider_supplementary, file = here("plots/spider_supplementary.tiff"), dpi = 500, width = 6, height = 6)
ggsave(spider_supplementary, file = here("plots/spider_supplementary.jpg"), dpi = 500, width = 6, height = 6)


