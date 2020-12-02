library(tidyverse)
library(ggrepel)
library(ggthemes)
library(cowplot)

d <- readRDS("data/pred-prey-mass.RDS")
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
                    sig1 = 0.00001,
                    sig2 = 0.01,
                    sig3 = 0.1,
                    sim = 1:N) %>%  # prior distribution of beta
  expand_grid(log_pred_mass = d$log_pred) %>% 
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


fake_data %>% 
  group_by(model, response) %>% 
  summarize(mean_prey_mass = mean(value))


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

# calculate medians of the 100 simulations
fake_data_medians <- fake_data %>% 
  group_by(sim, model) %>% 
  summarize(value = median(value))

d_sim <- fake_data %>% 
  filter(response == "logpreymasssims") %>% 
  group_by(sim, model) %>% 
  filter(log_pred_mass == median(log_pred_mass)) %>% 
  ungroup() %>% 
  filter(sim <= nsim) %>%
  mutate(model = case_when(model == "a" ~ "d",
                           model == "b" ~ "e",
                           model == "c" ~ "f")) %>% 
  filter(model == "d") %>% 
  ggplot(aes(x = sim, y = value)) + 
  # geom_point(data = fake_data_medians %>% filter(model == "c") %>% filter(sim <= nsim),size = 4) +
  geom_boxplot(aes(group = sim), outlier.shape = NA) +
  geom_point(shape = 21, size = 0.8, position = position_jitter(width = 0.05)) +
  # facet_grid(.~model, scales = "free_y") +
  geom_hline(data = reference_points %>%
               mutate(model = case_when(model == "a" ~ "d",
                                        model == "b" ~ "e",
                                        model == "c" ~ "f")) %>% 
               filter(model == "f"),
             aes(yintercept = y, color = reference)) +
  # coord_cartesian(xlim = c(-250, 250),
  #                 ylim = c(0, 1.5)) +
  geom_text_repel(data = reference_points %>%
                    mutate(model = case_when(model == "a" ~ "d",
                                             model == "b" ~ "e",
                                             model == "c" ~ "f")) %>% 
                    filter(model == "f"), aes(y = y, x = 12, 
                                              label = reference, color = reference),
                  angle = 0, vjust = -0.2, size = 3, direction = "y",
                  nudge_x = 0.1) +
  theme_classic() +
  scale_color_colorblind() +
  guides(color = F) +
  labs(y = "Predicted mass of prey log(g)\nfor an average-sized predator", 
       x = "Simulation") +
  scale_x_continuous(breaks = c(0:10), limits = c(1, 12))


e_sim <- fake_data %>% 
  filter(response == "logpreymasssims") %>% 
  group_by(sim, model) %>% 
  filter(log_pred_mass == median(log_pred_mass)) %>% 
  ungroup() %>% 
  filter(sim <= nsim) %>%
  mutate(model = case_when(model == "a" ~ "d",
                           model == "b" ~ "e",
                           model == "c" ~ "f")) %>% 
  filter(model == "e") %>% 
  ggplot(aes(x = sim, y = value)) + 
  # geom_point(data = fake_data_medians %>% filter(model == "c") %>% filter(sim <= nsim),size = 4) +
  geom_boxplot(aes(group = sim), outlier.shape = NA) +
  geom_point(shape = 21, size = 0.8, position = position_jitter(width = 0.05)) +
  # facet_grid(.~model, scales = "free_y") +
  geom_hline(data = reference_points %>%
               mutate(model = case_when(model == "a" ~ "d",
                                        model == "b" ~ "e",
                                        model == "c" ~ "f")) %>% 
               filter(model == "e"),
             aes(yintercept = y, color = reference)) +
  # coord_cartesian(xlim = c(-250, 250),
  #                 ylim = c(0, 1.5)) +
  geom_text(data = reference_points %>%
              mutate(model = case_when(model == "a" ~ "d",
                                       model == "b" ~ "e",
                                       model == "c" ~ "f")) %>% 
              filter(model == "e"), aes(y = y+3, x = 11.5, 
                                        label = reference, color = reference),
            angle = 0, vjust = -0.2, size = 3,
            nudge_x = 0.1) +
  theme_classic() +
  scale_color_colorblind() +
  guides(color = F) +
  labs(y = "", 
       x = "Simulation",
       title = "") +
  scale_x_continuous(breaks = c(0:10), limits = c(1, 12))



f_sim <- fake_data %>% 
  filter(response == "logpreymasssims") %>% 
  group_by(sim, model) %>% 
  filter(log_pred_mass == median(log_pred_mass)) %>% 
  ungroup() %>% 
  filter(sim <= nsim) %>%
  mutate(model = case_when(model == "a" ~ "d",
                           model == "b" ~ "e",
                           model == "c" ~ "f")) %>% 
  filter(model == "f") %>% 
  ggplot(aes(x = sim, y = value)) + 
  # geom_point(data = fake_data_medians %>% filter(model == "c") %>% filter(sim <= nsim),size = 4) +
  geom_boxplot(aes(group = sim), outlier.shape = NA) +
  geom_point(shape = 21, size = 0.8, position = position_jitter(width = 0.05)) +
  # facet_grid(.~model, scales = "free_y") +
  geom_hline(data = reference_points %>%
               mutate(model = case_when(model == "a" ~ "d",
                                        model == "b" ~ "e",
                                        model == "c" ~ "f")) %>% 
               filter(model == "f"),
             aes(yintercept = y, color = reference)) +
  # coord_cartesian(xlim = c(-250, 250),
  #                 ylim = c(0, 1.5)) +
  geom_text(data = reference_points %>%
              mutate(model = case_when(model == "a" ~ "d",
                                       model == "b" ~ "e",
                                       model == "c" ~ "f")) %>% 
              filter(model == "f"), aes(y = y+3, x = 11.5, 
                                        label = reference, color = reference),
            angle = 0, vjust = 0.2, size = 3,
            nudge_x = 0.1) +
  theme_classic() +
  scale_color_colorblind() +
  guides(color = F) +
  labs(y = "", 
       x = "Simulation",
       title = "") +
  scale_x_continuous(breaks = c(0:10), limits = c(1, 12))



sim_new_grid <- plot_grid(d_sim, e_sim, f_sim, nrow = 1, labels = c("d", "e", "f"), hjust = c(-6.5, 
                                                                              -4.5,
                                                                              -7.5), 
          align = "h")




# simulate regression lines
sim_reglines_a <- fake_data %>% 
  filter(response == "musims" & model == "a") %>% 
  mutate(mean = mean(value)) %>% 
  ggplot(aes(x = log_pred_mass, y = value)) + 
  geom_line(size = 0.2, aes(group = sim), alpha = 0.4) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  # geom_text_repel(data = reference_points,
  #                 aes(x = max(d$log_pred) + 4, 
  #                     y = y, 
  #                     label = reference),
  #                 hjust = -0.2,
  #                 vjust = 0.5,
  #                 direction = "y",
  #                 size = 3,
  #                 xlim = c(-25, 50),
  #                 ylim = c(-170,170)) +
  # facet_grid(.~model, scales = "free_y") +
  geom_text_repel(data = reference_points %>% filter(model == "a"),
            aes(x = max(d$log_pred) + 2,
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
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Mass of prey log(g)",
       x = "Mass of predator log(g)") +
  scale_x_continuous(breaks = c(-20, -10, 0, 10), limits = c(-20, 20)) +
  ylim(-40000, 40000) + 
  geom_vline(xintercept = -6.18, linetype = "dashed") +
  annotate("text", x = -6.1, y = -(40000 - 0.02*40000), label = "Average-sized predator", size = 2.5) + 
  NULL


sim_reglines_b <- fake_data %>% 
  filter(response == "musims" & model == "b") %>% 
  mutate(mean = mean(value)) %>% 
  ggplot(aes(x = log_pred_mass, y = value)) + 
  geom_line(size = 0.2, aes(group = sim), alpha = 0.4) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  # geom_text_repel(data = reference_points,
  #                 aes(x = max(d$log_pred) + 4, 
  #                     y = y, 
  #                     label = reference),
  #                 hjust = -0.2,
  #                 vjust = 0.5,
  #                 direction = "y",
  #                 size = 3,
  #                 xlim = c(-25, 50),
  #                 ylim = c(-170,170)) +
  # facet_grid(.~model, scales = "free_y") +
geom_text_repel(data = reference_points %>% filter(model == "a"),
                aes(x = max(d$log_pred) + 2,
                    y = y, label = reference,
                    color = reference),
                hjust = 0, 
                size = 2, 
                nudge_x = 0,
                direction = "y",
                nudge_y = 20)  +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "",
       x = "Mass of predator log(g)") +
  scale_x_continuous(breaks = c(-20, -10, 0, 10), limits = c(-20, 20)) + 
  ylim(-600, 600) +
  geom_vline(xintercept = -6.18, linetype = "dashed") +
  annotate("text", x = -6.1, y = -(600 - 0.02*600), label = "Average-sized predator", size = 2.5) +
  NULL


sim_reglines_c <- fake_data %>% 
  filter(response == "musims" & model == "c") %>% 
  mutate(mean = mean(value)) %>% 
  ggplot(aes(x = log_pred_mass, y = value)) + 
  geom_line(size = 0.2, aes(group = sim), alpha = 0.4) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  # geom_text_repel(data = reference_points,
  #                 aes(x = max(d$log_pred) + 4, 
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
                aes(x = max(d$log_pred) + 1,
                    y = y, label = reference,
                    color = reference),
                hjust = 0, 
                size = 3, 
                nudge_x = 0,
                direction = "y",
                nudge_y = 4) +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "",
       x = "Mass of predator log(g)") +
  scale_x_continuous(breaks = c(-20, -10, 0, 10), limits = c(-20, 20)) +
  ylim(-80, 80) +
  geom_vline(xintercept = -6.18, linetype = "dashed") +
  annotate("text", x = -6.1, y = -(80 - 0.02*80), label = "Average-sized predator", size = 2.5) +
  NULL


sim_reg_grid <- plot_grid(sim_reglines_a, sim_reglines_b, sim_reglines_c, 
                          nrow = 1, labels = c("a", "b", "c"), hjust = c(-4.5, -5.5, -3.5), 
                          align = "h")


mod_1 <- plot_grid(sim_reglines_a, sim_reglines_b, sim_reglines_c, 
                   d_sim, e_sim, f_sim, ncol = 3, labels = "auto")

# mod_1

saveRDS(mod_1, file = "plots/mod_1.rds")
ggsave(mod_1, file = "plots/mod_1.tiff", dpi = 400, width = 10, height = 6)
ggsave(mod_1, file = "plots/mod_1.jpg", dpi = 400, width = 10, height = 6)
