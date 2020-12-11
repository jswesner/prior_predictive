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


set.seed(1095)
N <- 100 # number of simulations
fake_data <- tibble(a = rnorm(N, 0, 1000), # prior distribution of alpha
                    b = rnorm(N, 0, 1000),
                    a2 = rnorm(N, 0, 10),
                    b2 = rnorm(N, 0, 10),
                    a3 = rnorm(N, 0, 5),
                    b3 = rnorm(N, 0, 5),
                    sig1 = 0.001,
                    sig2 = 0.01,
                    sig3 = 0.1,
                    sim = 1:N) %>%  # prior distribution of beta
  expand_grid(log_pred_mass = d_tongue$log_pred_c) %>% 
  mutate(mean_log_pred = d_tongue$mean_log_pred[1]) %>% 
  mutate(musims_1 = a + b*log_pred_mass,
         logpreymasssims_1 = musims_1 + rnorm(nrow(.), 0, rexp(1, min(sig1))),
         musims_2 = a2 + b2*log_pred_mass,
         logpreymasssims_2 = musims_2 + rnorm(nrow(.), 0, rexp(1, min(sig2))),
         musims_3 = a3 + b3*log_pred_mass,
         logpreymasssims_3 = musims_3 + rnorm(nrow(.), 0, rexp(1, min(sig3)))) %>% 
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
# test <- fake_data %>% 
#   filter(response == "musims" & sim <= nsim ) %>% 
#   group_by(sim, model) %>% 
#   mutate(sigma = case_when(model == "a" ~ rexp(1, sample(sig1, size =  1)),
#                            model == "b" ~ rexp(1, sample(sig2, size = 1)),
#                            model == "c" ~ rexp(1, sample(sig3, size = 1)))) %>% 
#   ungroup() %>% 
#   mutate(y_new = value + rnorm(nrow(.), 0, sigma)) %>% 
#   mutate(model = case_when(model == "a" ~ "d",
#                            model == "b" ~ "e",
#                            model == "c" ~ "f")) %>% 
#   # filter(model == "f") %>% 
#   ggplot(aes(x = log_pred_mass, y = y_new)) +
#   geom_point(size = 0.1, shape = 21) +
#   # geom_line(aes(group = sim), stat="density", alpha=0.1) +
#   facet_grid(model ~ sim, scales = "free_y") +
#   # coord_cartesian(ylim = c(-250, 250)) +
#   geom_hline(data = reference_points %>%
#                mutate(model = case_when(model == "a" ~ "d",
#                                         model == "b" ~ "e",
#                                         model == "c" ~ "f")),
#              aes(yintercept = y, color = reference)) +
#   geom_text_repel(data = reference_points %>% 
#               mutate(sim = nsim,
#                      model = case_when(model == "a" ~ "d",
#                                        model == "b" ~ "e",
#                                        model == "c" ~ "f")), aes(y = y, x = 2.8, 
#                                                                  label = reference, color = reference),
#             angle = 0, size = 3) +
#   theme_bw() +
#   # scale_color_colorblind() +
#   guides(color = F) +
#   labs(y = "Predicted mass of prey (log grams) for an average sized predator", 
#        title = "Ten simulated datasets from the prior predictive distribution") 



# number of sims to plot out of N
nsim = 10

# Conditional predictions of the size of prey for a median-sized predator
y_sims <- fake_data %>% 
  filter(response == "logpreymasssims") %>% 
  group_by(sim, model) %>% 
  filter(log_pred_mass == median(log_pred_mass)) 


y_sims_label <- "y[i] %~% N(mu[i],sigma)"
y_sims_text <- "(Simulated individual prey masses of\n an average-sized predator, n = 100)"

mu_sims_label <- "mu[i] == alpha + beta*x[i]"
mu_sims_text <- "(Simulated regression lines, n = 100)"
                         
# simulate regression lines

sim_reglines_a <- fake_data %>% 
  filter(response == "musims" & model == "a") %>% 
  mutate(mean = mean(value)) %>%
  ggplot(aes(x = log_pred_mass + mean_log_pred, y = value)) +  #un-center predictor
  geom_line(size = 0.2, aes(group = sim), alpha = 0.4) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  geom_text_repel(data = reference_points %>% filter(model == "a"),
                aes(x = max(d_tongue$log_pred) + 0.2,
                    y = y, label = reference,
                    color = reference),
                hjust = 0, 
                size = 3, 
                nudge_x = 0,
                direction = "y",
                nudge_y = 20) +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  scale_x_continuous(limits = c(-7.8, -6.1)) +
  labs(y = "Mass of prey log(g)",
       x = "Mass of predator log(g)",
       title = "Weak Priors",
       subtitle = expression(paste(alpha%~%N(0, 1000),", ", ~ beta%~%N(0, 1000),", ", ~ sigma%~%exp(0.001)))) +
  # coord_cartesian(ylim = c(-10000, 10000)) +
  annotate("text", x = median(d_tongue$log_pred) + 0.55, y = 10000, 
           label = as.character(y_sims_label), size = 4, 
           parse = T) +
  annotate("text", x = median(d_tongue$log_pred) + 0.55, y = 10000 - 2000, 
           label = y_sims_text, size = 3.5) +
  annotate("text", x = -6.55, y = -9000, 
           label = as.character(mu_sims_label), size = 4, 
           parse = T) + 
  annotate("text", x = -6.55, y = -9000-1500, 
           label = as.character(mu_sims_text), size = 3.5) +
  geom_segment(aes(x = -6.7, 
                   y = -9000, 
                   xend = -6.7, 
                   yend = -4000), 
               size=1.2,
               type = "closed",
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = -6.8, 
                   y = 10000, 
                   xend = median(d_tongue$log_pred) + 0.05, 
                   yend = 10000), 
               size = 1.2,
               type = "closed",
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_boxplot(data = y_sims %>% filter(model == "a"), aes(y = value), width = 0.05,
               outlier.shape = NA) +
  NULL


sim_reglines_b <- fake_data %>% 
  filter(response == "musims" & model == "b") %>% 
  mutate(mean = mean(value)) %>% 
  ggplot(aes(x = log_pred_mass + mean_log_pred, y = value)) + 
  geom_line(size = 0.2, aes(group = sim), alpha = 0.4) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  # geom_text_repel(data = reference_points,
  #                 aes(x = max(d_tongue$log_pred_c) + 4, 
  #                     y = y, 
  #                     label = reference),
  #                 hjust = -0.2,
  #                 vjust = 0.5,
  #                 direction = "y",
  #                 size = 3,
  #                 xlim = c(-25, 50),
  #                 ylim = c(-170,170)) +
  # facet_grid(.~model, scales = "free_y") +
geom_text(data = reference_points %>% filter(model == "c"),
          aes(x = max(d_tongue$log_pred) + .2,
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
       title = "Stronger Priors",
       subtitle = expression(paste(alpha%~%N(0, 10),", ", ~ beta%~%N(0, 10),", ", ~ sigma%~%exp(0.01)))) +
  scale_x_continuous(limits = c(-7.8, -6.1)) +
  ylim(-80, 80) +
  geom_boxplot(data = y_sims %>% filter(model == "b"), aes(y = value), width = 0.05,
               outlier.shape = NA) +
  NULL


sim_reglines_c <- fake_data %>% 
  filter(response == "musims" & model == "c") %>% 
  mutate(mean = mean(value)) %>% 
  ggplot(aes(x = log_pred_mass + mean_log_pred, y = value)) + 
  geom_line(size = 0.2, aes(group = sim), alpha = 0.8) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  # geom_text_repel(data = reference_points,
  #                 aes(x = max(d_tongue$log_pred_c) + 4, 
  #                     y = y, 
  #                     label = reference),
  #                 hjust = -0.2,
  #                 vjust = 0.5,
  #                 direction = "y",
  #                 size = 3,
  #                 xlim = c(-25, 50),
  #                 ylim = c(-170,170)) +
  # facet_grid(.~model, scales = "free_y") +
geom_text(data = reference_points %>% filter(model == "c"),
          aes(x = max(d_tongue$log_pred) + .2,
              y = y, label = reference,
              color = reference),
          hjust = 0, 
          size = 3, 
          nudge_x = -0.2,
          nudge_y = 1) +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "",
       x = "Mass of predator log(g)",
       title = "Strongest Priors",
       subtitle = expression(paste(alpha%~%N(0, 1),", ", ~ beta%~%N(0, 1),", ", ~ sigma%~%exp(0.1)))) +
  scale_x_continuous(limits = c(-7.8, -6.1)) +
  ylim(-30, 30) +
  geom_boxplot(data = y_sims %>% filter(model == "c"), aes(y = value), width = 0.05,
               outlier.shape = NA) +
  NULL


sim_reg_grid <- plot_grid(sim_reglines_a, sim_reglines_b, sim_reglines_c, 
                          nrow = 1, labels = c("a", "b", "c"),
                          align = "h")

sim_reg_grid

saveRDS(sim_reg_grid, file = "plots/mod_1.rds")
ggsave(sim_reg_grid, file = "plots/mod_1.tiff", dpi = 400, width = 15, height = 5)
ggsave(sim_reg_grid, file = "plots/mod_1.jpg", dpi = 400, width = 15, height = 5)
