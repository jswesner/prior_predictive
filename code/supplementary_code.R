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
d <- read.csv(here("data/pred-prey-mass.csv"))
spiders <- read.csv(here("data/spiders.csv"))


# plot raw data

d_plot <- d %>% 
  ggplot(aes(x = log_prey, y = log_pred)) +
  geom_point(alpha = 0.1, size = 0.3) +
  labs(y = "Prey Mass log(g)",
       x = "Predator Mass log(g)") +
  theme_classic() 

spiders_plot <- spiders %>% 
  mutate(date_date = ymd(date),
         Treatment = case_when(trt == "buffalo" ~ "Smallmouth Buffalo",
                               trt == "green" ~ "Green Sunfish",
                               TRUE ~ "Fishless Control")) %>% 
  ggplot(aes(x = date_date, y = webs, fill = Treatment, shape = Treatment)) + 
  geom_point(position = position_jitterdodge(jitter.height = 0.09, jitter.width = 0, dodge.width = 2.5),
             alpha = 0.7,
             size = 2) +
  scale_fill_colorblind() +
  scale_shape_manual(values = c(21, 22, 24)) +
  labs(y = "Number of Spiders",
       x = "Date") +
  theme_classic() 

data_plots <- plot_grid(d_plot, spiders_plot, ncol = 2, align = "h",
                        rel_widths = c(1, 1.6),
                        labels = "auto")

saveRDS(data_plots, file = "plots/data_plots.rds")
ggsave(data_plots, file = "plots/data_plots.tiff", dpi = 600, width = 7, height = 2.4)
ggsave(data_plots, file = "plots/Figure_1.tiff", dpi = 600, width = 7, height = 2.4)

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


saveRDS(spiders_wide_post, file = "models/spiders_wide_post.rds")
saveRDS(spiders_narrow_post, file = "models/spiders_narrow_post.rds")
saveRDS(spiders_narrowes_post, file = "models/spiders_narrowes_post.rds")


spiders_wide_post <- "models/spiders_wide_post.rds"
spiders_narrow_post <- "models/spiders_narrow_post.rds"
spiders_narrowes_post <- "models/spiders_narrowes_post.rds"

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
spider_supplementary <- readRDS(here("plots/spider_supplementary.rds"))
ggsave(spider_supplementary, file = here("plots/spider_supplementary.jpg"), dpi = 500, width = 6, height = 6)


