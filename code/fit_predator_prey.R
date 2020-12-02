library(tidyverse)
library(ggthemes)
library(rethinking)
library(cowplot)


# Fit predator prey model -------------------------------------------------

d <- readRDS(here("data/pred-prey-mass.RDS"))

d$log_pred <- log(d$pred)
d$log_prey <- log(d$prey)

m1 <- quap(
  alist(
    log_prey ~ dnorm(mu, sigma),
    mu <- a + b * log_pred,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
    sigma ~ dexp(0.1)
  ),
  data = d
)
precis(m1)

post <- extract.samples(m1, 20)

# Plot predator prey model ------------------------------------------------

fake_data <- readRDS(here("data/fake_data_predator_prey.rds")) %>% 
  mutate(prior_post = "Prior")

post_preds <- post %>% 
  mutate(sim = 1:nrow(.)) %>% 
  expand_grid(log_pred_mass = d$log_pred) %>% 
  mutate(musims = a + b*log_pred_mass,
         logpreymasssims = musims + rnorm(nrow(.), 0, rexp(1, 3.66))) %>% 
  mutate(prior_post = "Posterior") %>% 
  pivot_longer(cols = c("musims", "logpreymasssims"), names_to = "response") %>% 
  bind_rows(fake_data %>% filter(model == "c") %>% select(sim, log_pred_mass, prior_post, response, value)) %>% 
  mutate(prior_post = fct_relevel(prior_post, "Prior"))

post_pred_prey <- post_preds %>% 
  filter(response == "musims" & sim <= 100) %>% 
  ggplot(aes(x = log_pred_mass, y = value)) +
  geom_point(data = d, aes(y = log_prey, x = log_pred), 
             size = 0.5, shape = 21) +
  geom_line(aes(group = interaction(prior_post,sim), alpha = prior_post, color = prior_post)) +
  scale_color_colorblind() +
  scale_alpha_manual(values = c(0.1, 1)) +
  theme_classic() +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.title = element_blank()) +
  labs(y = "Mass of prey (log grams)",
       x = "Mass of predator (log grams)") +
  geom_vline(xintercept = -6.18, linetype = "dashed") +
  annotate("text", x = -6.1, y = -40, label = "Average-sized predator", size = 2.5) + 
  NULL


nsim = 10
postconditional_pred_prey <- post_preds %>% 
  filter(response == "logpreymasssims") %>% 
  group_by(sim, prior_post) %>% 
  filter(log_pred_mass == median(log_pred_mass)) %>% 
  ungroup() %>% 
  filter(sim <= nsim) %>%
  ggplot(aes(x = sim, y = value)) + 
  # geom_point(data = fake_data_medians %>% filter(model == "c") %>% filter(sim <= nsim),size = 4) +
  geom_boxplot(aes(group = interaction(sim, prior_post), color = prior_post), outlier.shape = NA) +
  geom_point(size = 0.4, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.1), 
             aes(shape = prior_post, color = prior_post)) +
  # facet_grid(.~model, scales = "free_y") +
  theme_classic() +
  scale_color_colorblind() +
  # guides(color = F) +
  labs(y = "Log prey mass (g)\nfor an average-sized predator", 
       x = "Simulation",
       title = "") +
  # coord_cartesian(ylim = c(-20, 10)) +
  scale_x_continuous(breaks = c(0:10)) +
  theme(legend.title = element_blank())

plot_fit_predprey <- plot_grid(post_pred_prey, postconditional_pred_prey, ncol = 1, align = "v",
                               labels = "auto")

saveRDS(plot_fit_predprey, file = here("plots/post_pred_prey.rds"))
ggsave(plot_fit_predprey, file = here("plots/post_pred_prey.tiff"), dpi = 400, width = 6, height = 6) 
ggsave(plot_fit_predprey, file = here("plots/post_pred_prey.jpg"), dpi = 400, width = 6, height = 6)









