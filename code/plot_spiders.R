library(tidyverse)
library(ggthemes)
library(rethinking)
library(cowplot)
library(lubridate)
library(brms)
library(janitor)
library(scales)

# load data 
spiders <- read_csv("https://raw.githubusercontent.com/jswesner/warmbold_wesner/master/webs.csv") %>% 
  mutate(date = mdy(date),
         datefac = as.factor(date)) %>% 
  rename(cage = eid) %>% 
  select(-X1) %>% 
  filter(trt != "both" & trt != "cagectrl")

# show parameters that need priors  
get_prior(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"))


# Fit Models --------------------------------------------------------------

# Sample prior only 
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


preds_all <- bind_rows(preds_wide, preds_narrow, preds_narrowest) %>% 
  mutate(facet = case_when(prior_type == "wide" ~ "a",
                           prior_type == "narrow" ~ "b",
                           prior_type == "narrowest" ~ "c"),
         date = as_date(date)) %>% 
  rename(Treatment = trt)


spiders_top <- preds_all %>% 
  ggplot(aes(x = date, y = value, fill = Treatment, group = interaction(Treatment, date))) +
  geom_violin(position = position_dodge(width = 5)) +
  scale_y_log10(labels = c("0.001","0.01", "1","10", "100", "1000", "100,000", "100,000,000"), 
                breaks = c(0.001, 0.01, 1, 10, 100, 1000, 100000, 100000000)) +
  scale_fill_colorblind() +
  facet_grid(.~facet) +
  coord_cartesian(ylim = c(0.001, 100000000)) + 
  theme_classic() +
  labs(y = "Mean Number of Spiders",
       x = "Date",
       title = "Simulated Mean Spider Abundance")
  

# Simulate number of spiders in a new cage --------------------------------

newdata <- data.frame(datefac = as.factor(max(spiders$date)),
                      trt = unique(spiders$trt),
                      cage = "new")

predsint_wide <- predict(spiders_wide, newdata = newdata, summary = F, allow_new_levels = T) %>% as_tibble() %>% mutate(sims = 1:nrow(.), facet = "c") 
predsint_narrow <- predict(spiders_narrow, newdata = newdata, summary = F, allow_new_levels = T) %>% as_tibble() %>% mutate(sims = 1:nrow(.), facet = "d")
predsint_narrowest <- predict(spiders_narrowest, newdata = newdata, summary = F, allow_new_levels = T) %>% as_tibble() %>% mutate(sims = 1:nrow(.), facet = "e")
predsint_all = bind_rows(predsint_wide, predsint_narrow, predsint_narrowest) %>% 
  pivot_longer(cols = c(V1, V2, V3)) %>% 
  mutate(trt = case_when(name == "V1" ~ "buffalo",
                         name == "V2" ~ "fishless",
                         TRUE ~ "green"))

spiders_bottom <- predsint_all %>% 
  ggplot(aes(x = trt, y = value)) + 
  geom_point(position = position_jitter(width = 0.1), shape = 21) +
  scale_y_log10(labels = c("1", "10", "100", "1,000", "100,000", "100,000,000"), breaks = c(1, 10, 100, 1000, 100000, 100000000)) +
  facet_grid(.~facet, scales = "free_y") +
  theme_classic() +
  labs(y = "Number of Spiders", 
       x = "Treatment",
       title = "Simulated Number of Spiders")
  
legend = get_legend(spiders_top + theme(legend.position = "top"))
spiders_priors <- plot_grid(legend, spiders_top +  guides(fill = F), spiders_bottom, ncol = 1, align = "h",
                            rel_heights = c(0.2, 1,1))
spiders_priors

library(here)
saveRDS(spiders_priors, file = here("plots/spiders_priors.rds"))
ggsave(spiders_priors, file = here("plots/spiders_priors.tiff"), dpi = 400, width = 6, height = 6)
ggsave(spiders_priors, file = here("plots/spiders_priors.jpg"), dpi = 400, width = 6, height = 6)


# Fit Models - Posterior --------------------------------------------------

spiders_narrow_post <- brm(webs ~ trt*datefac + (1|cage), data = spiders, family = poisson(link = "log"),
                      prior = c(prior(normal(0, 1), class = "b"),
                                prior(normal(0, 1), class = "Intercept"),
                                prior(exponential(1), class = "sd")),
                      chains = 1, iter = 1000) 

spider_post <- fitted(spiders_narrow_post, summary = F, scale = "response") %>% t() %>% as_tibble() %>% 
  mutate(trt= spiders$trt,
         cage = spiders$cage,
         date = spiders$date) %>% 
  pivot_longer(cols = c(-trt, -cage, -date)) %>% 
  clean_names() %>% 
  mutate(prior_post = "posterior",
         prior_type = "narrow",
         Treatment = trt)

post_plot <- bind_rows(preds_narrow, spider_post)


spider_plotdata <- spiders %>% mutate(Treatment = trt)


spider_post_top <- spider_post %>% 
ggplot(aes(x = date, y = value, group = interaction(Treatment, date))) +
  geom_violin(data = preds_narrow %>% mutate(Treatment = trt), 
              position = position_dodge(width = 5),alpha = 0.5) +
  geom_violin(position = position_dodge(width = 5), aes(fill = Treatment)) +
  geom_point(data = spider_plotdata , 
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 5),
             aes(y = webs, fill = Treatment), color = "grey50", alpha = 0.5) +
  scale_y_log10(labels = c("0.001","0.01", "1","10", "100", "1000", "100,000"), 
                breaks = c(0.001, 0.01, 1, 10, 100, 1000, 100000)) +
  scale_fill_colorblind() +
  coord_cartesian(ylim = c(0.001, 100000)) + 
  theme_classic() +
  labs(y = "Mean Number of Spiders",
       x = "Date") +
  theme(legend.position = "top")


# new post predictions
spiderint_post <- predict(spiders_narrow_post, newdata = newdata, 
                          summary = F, allow_new_levels = T) %>% 
  as_tibble() %>% mutate(sims = 1:nrow(.), facet = "e") %>% 
  pivot_longer(cols = c(V1, V2, V3)) %>% 
  mutate(trt = case_when(name == "V1" ~ "buffalo",
                         name == "V2" ~ "fishless",
                         TRUE ~ "green"),
         prior_post = "Posterior")

spiderint_all <- bind_rows(spiderint_post, predsint_narrow %>% pivot_longer(cols = c(V1, V2, V3)) %>% 
                             mutate(trt = case_when(name == "V1" ~ "buffalo",
                                                    name == "V2" ~ "fishless",
                                                    TRUE ~ "green"),
                                    prior_post = "Prior"))
  
spider_post_bottom <- spiderint_all %>% 
  mutate(prior_post = fct_relevel(prior_post, "Prior")) %>% 
  ggplot(aes(x = trt, y = value, alpha = prior_post)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.4,
                                             jitter.width = 0.1), shape = 21,
             aes(fill = prior_post)) +
  scale_fill_manual(values = c("grey10", "white")) +
  theme_classic() +
  scale_y_log10(labels = comma) +
  labs(y = "Number of Spiders", 
       x = "Treatment") +
  theme(legend.title = element_blank(),
        legend.positio = "top") +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  NULL

spider_post_plot <- plot_grid(spider_post_top, spider_post_bottom,
                              ncol = 1, labels = "auto")

saveRDS(spider_post_plot, file = here("plots/spider_post_plot.rds"))
ggsave(spider_post_plot, file = here("plots/spider_post_plot.tiff"), dpi = 400, width = 6, height = 6)
ggsave(spider_post_plot, file = here("plots/spider_post_plot.jpg"), dpi = 400, width = 6, height = 6)


spider_post %>% 
  group_by(trt, date) %>% 
  summarize(mean = mean(value))
                         