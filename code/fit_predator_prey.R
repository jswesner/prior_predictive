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
       x = "Mass of predator (log grams)") 

postconditional_pred_prey <- post_preds %>% 
  filter(response == "logpreymasssims" & sim <= 100) %>% 
  group_by(prior_post) %>% 
  filter(log_pred_mass == median(log_pred_mass)) %>% 
  ggplot(aes(x = value, y = ..scaled..)) + 
  geom_line(aes(group = interaction(prior_post, sim), alpha = prior_post, color = prior_post), stat="density") +
  theme_classic() +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.title = element_blank()) +
  scale_color_colorblind() +
  scale_alpha_manual(values = c(0.1, 1)) +
  labs(x = "Predicted mass of prey (log grams) for an average sized predator") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

plot_fit_predprey <- plot_grid(post_pred_prey, postconditional_pred_prey, ncol = 1, align = "v",
                               labels = "auto")

saveRDS(plot_fit_predprey, file = here("plots/post_pred_prey.rds"))
ggsave(plot_fit_predprey, file = here("plots/post_pred_prey.tiff"), dpi = 400, width = 6, height = 6) 
ggsave(plot_fit_predprey, file = here("plots/post_pred_prey.jpg"), dpi = 400, width = 6, height = 6)






