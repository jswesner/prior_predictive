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

# load data for plots (don't need anything else but this. non-plotting code below just recreates these files)

letters_preds <- readRDS(file = "data/letters_preds.rds")
preds_all_toplot <- readRDS(file = "data/preds_all_toplot.rds")
letters_predsint <- readRDS(file = "data/letters_predsint.rds")
preds_int_all_toplot <- readRDS(file = "data/preds_int_all_toplot.rds")


# sKIP TO 'Make Plots' after loading the files above.

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

spiders_wide <- readRDS(here("models/spiders_wide.rds"))
spiders_narrow <- readRDS(here("models/spiders_narrow.rds"))
spiders_narrowest <- readRDS(here("models/spiders_narrowest.rds"))

# Fit the model to data ---------------------------------------------------

# This step is done here only to facilitate making the plots in the main text. Otherwise, this step would wait
# until after choosing priors.
spiders_narrow_post <- brm(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"),
                      prior = c(prior(normal(0, 1), class = "b"),
                                prior(normal(0, 1), class = "Intercept"),
                                prior(exponential(1), class = "sd")),
                      chains = 1, iter = 1000)

saveRDS(spiders_narrow_post, file = "models/spiders_narrow_post.rds")

spiders_narrow_post <- readRDS(here("models/spiders_narrow_post.rds"))

spider_post <- fitted(spiders_narrow_post, summary = F, scale = "response") %>% t() %>% as_tibble() %>% 
  mutate(trt= spiders$trt,
         cage = spiders$cage,
         date = spiders$date) %>% 
  pivot_longer(cols = c(-trt, -cage, -date)) %>% 
  clean_names() %>% 
  mutate(prior_post = "posterior",
         prior_type = "narrow",
         Treatment = trt)




# Make Plot Data --------------------------------------------------------------

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



# Simulate number of spiders in a new cage 

# data to simulate across
newdata <- data.frame(datefac = as.factor(max(as.Date(spiders$date))),
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



letters_predsint <- tibble(trt = c("Buffalo", "Buffalo", "Buffalo"), 
                           value = c(1e+94, 1e+07,
                                     100),
                           label = c("b)", "d)", "f)")) %>% 
  mutate(facet = c("Weak Priors",
                   "Stronger Priors", 
                   "Strongest Priors"),
         prior_post = "Posterior") %>% 
  mutate(facet = fct_relevel(facet, "Weak Priors", "Stronger Priors"))


preds_int_all_toplot <- predsint_all  %>% 
  mutate(facet = case_when(facet == "c" ~ "Weak Priors",
                           facet == "d" ~ "Stronger Priors",
                           TRUE ~ "Strongest Priors")) %>% 
  mutate(prior_post = "Prior") %>% 
  bind_rows(
    spider_pred_post %>% mutate(facet = "Stronger Priors")) %>% 
  mutate(prior_post = fct_relevel(prior_post, "Prior")) %>% 
  mutate(facet = fct_relevel(facet, "Weak Priors", "Stronger Priors"),
         trt = case_when(trt == "buffalo" ~ "Buffalo",
                         trt == "fishless" ~ "Fishless",
                         trt == "green" ~ "Green"))



letters_preds <- tibble(date = as.Date("2015-06-08"), 
                        value = 19600) %>% 
  expand_grid(label = c("a)", "c)", "e)")) %>% 
  mutate(facet = c("Weak Priors",
                   "Stronger Priors", 
                   "Strongest Priors"),
         Treatment = NA) %>% 
  mutate(facet = fct_relevel(facet, "Weak Priors", "Stronger Priors"))




preds_all_toplot <- preds_all %>% 
  mutate(prior_post = "Prior") %>% 
  bind_rows(spider_post %>% mutate(date = as.Date(date),
                                   prior_post = "Posterior",
                                   facet = "Stronger Priors") %>% 
              select(-trt, -prior_type)) %>% 
  mutate(facet = case_when(facet == "a" ~ "Weak Priors",
                           facet == "b" ~ "Stronger Priors",
                           facet == "c" ~ "Strongest Priors",
                           TRUE ~ "Stronger Priors")) %>% 
  mutate(facet = fct_relevel(facet, "Weak Priors", "Stronger Priors"),
         Treatment = case_when(Treatment == "buffalo" ~ "Buffalo",
                               Treatment == "fishless" ~ "Fishless",
                               Treatment == "green" ~ "Green"))


saveRDS(letters_preds, file = "data/letters_preds.rds")
saveRDS(preds_all_toplot, file = "data/preds_all_toplot.rds")
saveRDS(letters_predsint, file = "data/letters_predsint.rds")
saveRDS(preds_int_all_toplot, file = "data/preds_int_all_toplot.rds")


# Make Plots --------------------------------------------------------------



left <- preds_all_toplot %>% 
  filter(prior_post == "Prior") %>%
ggplot(aes(x = date, y = value, fill = Treatment, group = interaction(Treatment, date))) +
  scale_y_log10() +
  geom_violin(position = position_dodge(width = 5)) +
  geom_text(data = letters_preds , aes(label = label), nudge_x = -4) +
  # scale_y_log10(labels = c("0.000001", "0.0001", "0.01", "1", "100", "10,000", "1,000,000"),
  #               breaks = c(1/1000000, 1/10000, 1/100, 1, 100, 10000, 1000000)) +
  scale_fill_grey() +
  facet_grid(facet ~ ., ) +
  coord_cartesian(ylim = c(0.000051, 1/0.000051)) +
  theme_classic() +
  labs(y = "Mean Number of Spiders",
       title = "Simulated Means",
       subtitle = expression(paste(lambda["i"], " = ", alpha + beta[1]*x["ijt"],"...")),
       x = "Date") +
  theme(legend.position = "top",
        text = element_text(size = 11),
        legend.title = element_blank(),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        legend.key.size = unit(0.7,"line")) +
  geom_violin(data = preds_all_toplot %>% filter(prior_post != "Prior"),
              fill = "#E69F00",position = position_dodge(width = 5)) +
  NULL



 right <- preds_int_all_toplot %>% 
  ggplot(aes(x = trt, y = value)) + 
  geom_point(aes(color = prior_post), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.4), 
             alpha = 1,
             size = 0.4) +
  # scale_y_log10(labels = c("1", "10", "100", "1,000", "100,000", "100,000,000"), breaks = c(1, 10, 100, 1000, 100000, 100000000)) +
  facet_grid(facet ~., scales = "free_y", switch = "x") +
  scale_color_colorblind() +
  scale_y_log10() +
  geom_text(data = letters_predsint, aes(label = label), nudge_x = -0.4) +
  theme_classic() +
  labs(y = "Number of Spiders", 
       x = "Treatment", 
       title = "Simulated Data",
       subtitle = expression(paste(y["i"]^{"new"}, " ~ Poisson(", lambda["i"],")"))) +
  theme(text = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  NULL


spider_plot <- plot_grid(left, right, ncol = 2, align = "v")

ggsave(spider_plot, file = "plots/Figure 3.pdf", dpi = 600, width = 6, height = 6)


# Old Plots ---------------------------------------------------------------


# spiders_top_a <- preds_all %>% filter(facet == "a") %>% 
#   ggplot(aes(x = date, y = value, fill = Treatment, group = interaction(Treatment, date))) +
#   geom_violin(position = position_dodge(width = 5)) +
#   scale_y_log10() +
#   # scale_y_log10(labels = c("0.000001", "0.0001", "0.01", "1", "100", "10,000", "1,000,000"),
#   #               breaks = c(1/1000000, 1/10000, 1/100, 1, 100, 10000, 1000000)) +
#   scale_fill_grey() +
#   # facet_grid(.~facet) +
#   coord_cartesian(ylim = c(0.00001, 1000000)) + 
#   theme_classic() +
#   labs(y = "Mean Number of Spiders",
#        x = "Date",
#        title = expression(paste("Simulated Mean: ",lambda["i"], " = ", alpha + beta[1]*x["ijt"],"...")),
#        subtitle = "Weak Priors") +
#   theme(legend.position = "top",
#         text = element_text(size = 11),
#         legend.title = element_blank(),
#         plot.title = element_text(size = 10)) 
#   
# 
# spiders_top_b <- preds_all %>% filter(facet == "b") %>% 
#   ggplot(aes(x = date, y = value, fill = Treatment, group = interaction(Treatment, date))) +
#   geom_violin(position = position_dodge(width = 5)) +
#   scale_y_log10() +
#   # scale_y_log10(labels = c("0.000001", "0.0001", "0.01", "1", "100", "10,000", "1,000,000"),
#   #               breaks = c(1/1000000, 1/10000, 1/100, 1, 100, 10000, 1000000)) +
#   scale_fill_grey() +
#   # facet_grid(.~facet) +
#   coord_cartesian(ylim = c(0.00001, 1000000)) + 
#   theme_classic() +
#   labs(y = "Mean Number of Spiders",
#        x = "Date",
#        title = "",
#        subtitle = "Stronger Priors (with Posterior)") +
#   theme(legend.position = "top",
#         text = element_text(size = 11),
#         legend.title = element_blank()) +
#   geom_violin(data = spider_post %>% mutate(date = as.Date(date)), fill = "#E69F00",position = position_dodge(width = 5)) # add posterior
# 
# 
# spiders_top_c <- preds_all %>% filter(facet == "c") %>% 
#   ggplot(aes(x = date, y = value, fill = Treatment, group = interaction(Treatment, date))) +
#   geom_violin(position = position_dodge(width = 5)) +
#   scale_y_log10() +
#   # scale_y_log10(labels = c("0.000001", "0.0001", "0.01", "1", "100", "10,000", "1,000,000"),
#   #               breaks = c(1/1000000, 1/10000, 1/100, 1, 100, 10000, 1000000)) +
#   scale_fill_grey() +
#   # facet_grid(.~facet) +
#   coord_cartesian(ylim = c(0.00001, 1000000)) + 
#   theme_classic() +
#   labs(y = "Mean Number of Spiders",
#        x = "Date",
#        title = "",
#        subtitle = "Strongest Priors") +
#   theme(legend.position = "top",
#         text = element_text(size = 11),
#         legend.title = element_blank())
# 
# 
# 
# 
# 
# 
# # Make plots
# spiders_bottom_a <- predsint_all %>% filter(facet == "c") %>% 
#   ggplot(aes(x = trt, y = value)) + 
#   geom_point(position = position_jitter(width = 0.1), shape = 21) +
#   # scale_y_log10(labels = c("1", "10", "100", "1,000", "100,000", "100,000,000"), breaks = c(1, 10, 100, 1000, 100000, 100000000)) +
#   # facet_grid(.~facet, scales = "free_y") +
#   scale_y_log10() +
#   theme_classic() +
#   labs(y = "Number of Spiders", 
#        x = "Treatment",
#        title = expression(paste("Simulated Data: ",y["i"]^{"new"}, " ~ Poisson(", lambda["i"],")")),
#        subtitle = "Weak Priors") +
#   theme(text = element_text(size = 11),
#         plot.title = element_text(size = 10))
#   
# 
# spiders_bottom_b <- predsint_all %>% filter(facet == "d") %>% 
#   mutate(prior_post = "Prior") %>% 
#   bind_rows(spider_pred_post) %>% # combine with posterior predictions
#   mutate(prior_post = fct_relevel(prior_post, "Prior")) %>% 
#   ggplot(aes(x = trt, y = value, color = prior_post)) + 
#   geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.4), shape = 21) +
#   # scale_y_log10(labels = c("1", "10", "100", "1,000", "100,000", "100,000,000"), breaks = c(1, 10, 100, 1000, 100000, 100000000)) +
#   scale_y_log10() +
#   # facet_grid(.~facet, scales = "free_y") +
#   scale_color_colorblind() +
#   theme_classic() +
#   labs(y = "Number of Spiders", 
#        x = "Treatment",
#        title = "",
#        subtitle = "Stronger Priors (with Posterior)") +
#   geom_point(data = spider_pred_post, position = position_jitter(width = 0.1), shape = 21,
#              color = "#E69F00") +
#   theme(text = element_text(size = 11)) +
#   guides(color = F) +
#   NULL
# 
# spiders_bottom_c <- predsint_all %>% filter(facet == "e") %>%  
#   ggplot(aes(x = trt, y = value)) + 
#   geom_point(position = position_jitter(width = 0.1), shape = 21) +
#   # scale_y_log10(labels = c("1", "10", "100", "1,000", "100,000", "100,000,000"), breaks = c(1, 10, 100, 1000, 100000, 100000000)) +
#   scale_y_log10() +
#   # facet_grid(.~facet, scales = "free_y") +
#   theme_classic() +
#   labs(y = "Number of Spiders", 
#        x = "Treatment",
#        title = "",
#        subtitle = "Strongest Priors") +
#   theme(text = element_text(size = 11))
# 
# 

# # combine plots
# legend_spiders <- get_legend(spiders_top_a)
# 
# spiders_priors <- plot_grid(legend_spiders,
#                             spiders_top_a + guides(fill = F),
#                             spiders_bottom_a,
#                             spiders_top_b + guides(fill = F),
#                             spiders_bottom_b,
#                             spiders_top_c + guides(fill = F),
#                             spiders_bottom_c, ncol = 2, align = "v")
# 
# saveRDS(spiders_priors, file = here("plots/spiders_priors.rds"))
# ggsave(spiders_priors, file = here("plots/Figure 3.tiff"), dpi = 600, width = 6, height = 9)
# ggsave(spiders_priors, file = here("plots/spiders_priors.jpg"), dpi = 400, width = 16, height = 11)
# 

