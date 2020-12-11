library(tidyverse)
library(ggthemes)
library(rethinking)
library(cowplot)
library(here)
library(brms)
library(readr)
library(ggrepel)


d <- readRDS(here("data/pred-prey-mass.RDS"))

d$log_pred <- log(d$pred)
d$log_prey <- log(d$prey)

set.seed(1095)
N <- 100 # number of simulations
fake_data <- tibble(a = rnorm(N, 0, 1000), # prior distribution of alpha
                    b = rnorm(N, 0, 1000),
                    a2 = rnorm(N, 0, 10),
                    b2 = rnorm(N, 0, 10),
                    a3 = rnorm(N, 0, 1),
                    b3 = rnorm(N, 0, 1),
                    sig1 = 0.0001,
                    sig2 = 0.01,
                    sig3 = 0.1,
                    sim = 1:N) %>%  # prior distribution of beta
  expand_grid(log_pred_mass = d$log_pred) %>% 
  mutate(musims_1 = a + b*log_pred_mass,
         logpreymasssims_1 = musims_1 + rnorm(nrow(.), 0, rexp(nrow(.), min(sig1))),
         musims_2 = a2 + b2*log_pred_mass,
         logpreymasssims_2 = musims_2 + rnorm(nrow(.), 0, rexp(nrow(.), min(sig2))),
         musims_3 = a3 + b3*log_pred_mass,
         logpreymasssims_3 = musims_3 + rnorm(nrow(.), 0, rexp(nrow(.), min(sig3)))) %>% 
  pivot_longer(cols = c(musims_1, logpreymasssims_1, musims_2, logpreymasssims_2,
                        musims_3, logpreymasssims_3)) %>% 
  separate(name, c("response", "model")) %>% 
  mutate(model = case_when(model == 1 ~ "a",
                           model == 2 ~ "b",
                           TRUE ~ "c"))

reference_points <- tibble(reference = c("Whale", "Virus", "Earth", "Atom"),
                           y = c(18.18, -20.7, 63, -52)) %>%
  mutate(response = "logpreymassims") %>% 
  expand_grid(model = c("a", "b","c")) 

saveRDS(fake_data, file = here("data/fake_data_predator_prey.rds"))


# Make Plots --------------------------------------------------------------

nsim = 10
set.seed(2342908)


# Conditional predictions of the size of prey for a median-sized predator
y_sims <- fake_data %>% 
  filter(response == "logpreymasssims") %>% 
  group_by(sim, model) %>% 
  filter(log_pred_mass == median(log_pred_mass)) 


y_sims_label <- "y[i] %~% N(mu[i],sigma)"
y_sims_text <- "(Simulated individual prey masses for\n an average-sized predator, n = 100)"

mu_sims_label <- "mu[i] == alpha + beta*x[i]"
mu_sims_text <- "(Simulated regression lines, n = 100)"
                         
# simulate regression lines

sim_reglines_a <- fake_data %>% 
  filter(response == "musims" & model == "a") %>%
  group_by(sim) %>% 
  slice(seq(1, n(), by = 25)) %>%
  ggplot(aes(x = log_pred_mass, y = value)) + 
  geom_line(size = 0.2, aes(group = sim), alpha = 0.4) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  geom_text_repel(data = reference_points %>% filter(model == "a"),
                aes(x = max(d$log_pred) +  2 + rnorm(4, 0.2),
                    y = y, label = reference,
                    color = reference),
                hjust = 0, 
                size = 3, 
                nudge_x = 0,
                direction = "y") +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # scale_x_continuous(limits = c(-7.8, -6.1)) +
  labs(y = "Mass of prey log(g)",
       x = "Mass of predator log(g)",
       title = "Prior with weak priors",
       subtitle = expression(paste(alpha%~%N(0, 1000),", ", ~ beta%~%N(0, 1000),", ", ~ sigma%~%exp(0.0001)))) +
  scale_x_continuous(limits = c(-20, 20)) +
  annotate("text", x = median(d$log_pred) + 10, y = 37000, 
           label = as.character(y_sims_label), size = 3, 
           parse = T) +
  annotate("text", x = median(d$log_pred) + 10, y = 49000, 
           label = y_sims_text, size = 2.5) +
  annotate("text", x = 4, y = -37000, 
           label = as.character(mu_sims_label), size = 3, 
           parse = T) + 
  annotate("text", x = 4, y = -44000, 
           label = as.character(mu_sims_text), size = 2.5) +
  geom_segment(aes(x = 4, 
                   y = -34000, 
                   xend = 4, 
                   yend = -11000), 
               size = 0.5,
               type = "closed",
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(xend = -6.18 + 0.3, 
                   y = 40000, 
                   x = median(d$log_pred) + 5, 
                   yend = 20000), 
               size = 0.5,
               type = "closed",
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_boxplot(data = y_sims %>% filter(model == "a"), aes(y = value),
               outlier.shape = NA) +
  ylim(-60000, 60000) +
  NULL


sim_reglines_b <- fake_data %>% 
  filter(response == "musims" & model == "b") %>%
  group_by(sim) %>% 
  slice(seq(1, n(), by = 25)) %>% 
  mutate(mean = mean(value)) %>% 
  ggplot(aes(x = log_pred_mass, y = value)) + 
  geom_line(size = 0.2, aes(group = sim), alpha = 0.4) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
geom_text(data = reference_points %>% filter(model == "c"),
          aes(x = max(d$log_pred) + 2,
              y = y, label = reference,
              color = reference),
          hjust = 0, 
          size = 3, 
          nudge_x = -0.2,
          nudge_y = 12) +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "",
       x = "Mass of predator log(g)",
       title = "Prior with stronger priors",
       subtitle = expression(paste(alpha%~%N(0, 10),", ", ~ beta%~%N(0, 10),", ", ~ sigma%~%exp(0.01)))) +
  scale_x_continuous(limits = c(-20, 20)) +
  ylim(-500, 500) +
  geom_boxplot(data = y_sims %>% filter(model == "b"), aes(y = value), outlier.shape = NA) +
  NULL


sim_reglines_c <- fake_data %>% 
  filter(response == "musims" & model == "c") %>%
  group_by(sim) %>% 
  slice(seq(1, n(), by = 25)) %>% 
  mutate(mean = mean(value)) %>% 
  ggplot(aes(x = log_pred_mass, y = value)) + 
  geom_line(size = 0.2, aes(group = sim), alpha = 0.8) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  geom_text(data = reference_points %>% filter(model == "c"),
          aes(x = max(d$log_pred) + 2,
              y = y, label = reference,
              color = reference),
          hjust = 0, 
          size = 3, 
          nudge_x = -0.2,
          nudge_y = 3) +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "",
       x = "Mass of predator log(g)",
       title = "Prior with strongest priors",
       subtitle = expression(paste(alpha%~%N(0, 1),", ", ~ beta%~%N(0, 1),", ", ~ sigma%~%exp(0.1)))) +
  scale_x_continuous(limits = c(-20, 20)) +
  ylim(-80, 80) +
  geom_boxplot(data = y_sims %>% filter(model == "c"), aes(y = value),
               outlier.shape = NA) +
  NULL




# Plot prior v post -------------------------------------------------------
a <- readRDS(here("models/weak_brm_d.rds"))
b <- readRDS(here("models/stronger_brm_d.rds"))
c <- readRDS(here("models/strongest_brm_d.rds"))

a_cond <- conditional_effects(a)
b_cond <- conditional_effects(b)
c_cond <- conditional_effects(c)

a_summary <- a_cond$log_pred
b_summary <- b_cond$log_pred
c_summary <- c_cond$log_pred

# predictions for an average-sized predator (well, median-sized)
a_posts <- posterior_samples(a) %>% as_tibble() %>% clean_names() %>% mutate(iter = 1:nrow(.), 
                                                                         y_sims = b_intercept + b_log_pred*median(d$log_pred) + rnorm(nrow(.), 0, sigma))

b_posts <- posterior_samples(b) %>% as_tibble() %>% clean_names() %>% mutate(iter = 1:nrow(.), 
                                                                             y_sims = b_intercept + b_log_pred*median(d$log_pred) + rnorm(nrow(.), 0, sigma))

c_posts <- posterior_samples(c) %>% as_tibble() %>% clean_names() %>% mutate(iter = 1:nrow(.), 
                                                                             y_sims = b_intercept + b_log_pred*median(d$log_pred) + rnorm(nrow(.), 0, sigma))

a_conditional <- a_summary %>% 
  ggplot(aes(x = log_pred, y = estimate__)) + 
  geom_point(data = d, aes(x = log_pred, y = log_prey), size = 0.05, shape = 21, alpha = 0.2) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) +
  guides(color = F) +
  theme_classic() +
  # geom_point(data = d, aes(x = log_pred, y = log_prey)) +
  # scale_x_continuous(limits = c(-7.8, -6.1)) +
  labs(y = "Mass of prey log(g)",
       x = "Mass of predator log(g)",
       title = "Posterior with weak priors") +
  scale_x_continuous(limits = c(-20, 20)) +
  ylim(-25, 25) +
  geom_boxplot(data = a_posts, aes(y = y_sims, x = median(d$log_pred))) +
  annotate("text", x = -4, y = 17, label = paste("Median:", round(median(a_posts$y_sims), 1)), hjust = 0, size = 3) +
  annotate("text", x = -4, y = 18.5, label = paste("Max:", round(max(a_posts$y_sims), 1)), hjust = 0, size = 3) +
  annotate("text", x = -4, y = 15.5, label = paste("Min:", round(min(a_posts$y_sims), 1)), hjust = 0, size = 3) +
  geom_curve(x = -2, y = 14.5, xend = -6, yend = 0, curvature = -0.5,
             arrow = arrow(length = unit(0.5, "cm"))) +
  NULL


b_conditional <- b_summary %>% 
  ggplot(aes(x = log_pred, y = estimate__)) + 
  geom_point(data = d, aes(x = log_pred, y = log_prey), size = 0.05, shape = 21, alpha = 0.2) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) +
  guides(color = F) +
  theme_classic() +
  # geom_point(data = d, aes(x = log_pred, y = log_prey)) +
  # scale_x_continuous(limits = c(-7.8, -6.1)) +
  labs(y = "Mass of prey log(g)",
       x = "Mass of predator log(g)",
       title = "Posterior with stronger priors") +
  scale_x_continuous(limits = c(-20, 20)) +
  ylim(-25, 25) +
  geom_boxplot(data = b_posts, aes(y = y_sims, x = median(d$log_pred))) +
  annotate("text", x = -4, y = 17, label = paste("Median:", round(median(b_posts$y_sims), 1)), hjust = 0, size = 3) +
  annotate("text", x = -4, y = 18.5, label = paste("Max:", round(max(b_posts$y_sims), 1)), hjust = 0, size = 3) +
  annotate("text", x = -4, y = 15.5, label = paste("Min:", round(min(b_posts$y_sims), 1)), hjust = 0, size = 3) +
  geom_curve(x = -2, y = 14.5, xend = -6, yend = 0, curvature = -0.5,
             arrow = arrow(length = unit(0.5, "cm"))) +
  NULL


c_conditional <- c_summary %>% 
  ggplot(aes(x = log_pred, y = estimate__)) + 
  geom_point(data = d, aes(x = log_pred, y = log_prey), size = 0.05, shape = 21, alpha = 0.2) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) +
  guides(color = F) +
  theme_classic() +
  # geom_point(data = d, aes(x = log_pred, y = log_prey)) +
  # scale_x_continuous(limits = c(-7.8, -6.1)) +
  labs(y = "Mass of prey log(g)",
       x = "Mass of predator log(g)",
       title = "Posterior with strongest priors") +
  scale_x_continuous(limits = c(-20, 20)) +
  ylim(-25, 25) +
  geom_boxplot(data = c_posts, aes(y = y_sims, x = median(d$log_pred))) +
  annotate("text", x = -4, y = 17, label = paste("Median:", round(median(c_posts$y_sims), 1)), hjust = 0, size = 3) +
  annotate("text", x = -4, y = 18.5, label = paste("Max:", round(max(c_posts$y_sims), 1)), hjust = 0, size = 3) +
  annotate("text", x = -4, y = 15.5, label = paste("Min:", round(min(c_posts$y_sims), 1)), hjust = 0, size = 3) +
  geom_curve(x = -2, y = 14.5, xend = -6, yend = 0, curvature = -0.5,
             arrow = arrow(length = unit(0.5, "cm"))) +
  NULL


# All together now --------------------------------------------------------


sim_reg_grid <- plot_grid(sim_reglines_a, sim_reglines_b, sim_reglines_c,
                          a_conditional, b_conditional, c_conditional,
                          ncol = 3,labels = "auto")


saveRDS(sim_reg_grid, file = "plots/mod_1.rds")
ggsave(sim_reg_grid, file = "plots/mod_1.tiff", dpi = 400, width = 15, height = 10)
ggsave(sim_reg_grid, file = "plots/mod_1.jpg", dpi = 400, width = 15, height = 10)


