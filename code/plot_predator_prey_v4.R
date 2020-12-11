library(tidyverse)
library(ggthemes)
library(rethinking)
library(cowplot)
library(here)
library(brms)
library(readr)
library(ggrepel)
library(janitor)


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
                    sig3 = 0.5,
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
  labs(y = "Simulated prey mass log(g) ",
       x = "Mass of predator log(g)",
       title = expression(paste("100 prior simulations of means (",mu["i"], " = ", alpha, " + ", beta, "x"["i"],")")),
       subtitle = expression(paste("Weak priors: ", alpha%~%N(0, 1000),", ", ~ beta%~%N(0, 1000),", ", ~ sigma%~%exp(0.0001)))) +
  scale_x_continuous(limits = c(-20, 20)) +
  # annotate("text", x = -3, y = 60000, size = 3,
  #          label = expression(paste("Weak priors: ", alpha%~%N(0, 1000),", ", ~ beta%~%N(0, 1000),", ", ~ sigma%~%exp(0.0001)))) +
  ylim(-60000, 60000) +
  theme(plot.subtitle = element_text(size = 6),
        plot.title = element_text(size = 8)) +
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
          nudge_y = 15) +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Simulated prey mass log(g)",
       x = "Mass of predator log(g)",
       title = "",
       subtitle = expression(paste("Stronger priors: ", alpha%~%N(0, 10),", ", ~ beta%~%N(0, 10),", ", ~ sigma%~%exp(0.01)))) +
  scale_x_continuous(limits = c(-20, 20)) +
  # annotate("text", x = -3, y = 3000, size = 3,
  #          label = expression(paste("Stronger priors: ", alpha%~%N(0, 10),", ", ~ beta%~%N(0, 10),", ", ~ sigma%~%exp(0.01)))) +
  ylim(-1000, 1000) +
  theme(plot.subtitle = element_text(size = 6)) +
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
          nudge_y = 5) +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Simulated prey mass log(g)",
       x = "Mass of predator log(g)",
       title = "",
       subtitle = expression(paste("Strongest priors: ", alpha%~%N(0, 1),", ", ~ beta%~%N(0, 1),", ", ~ sigma%~%exp(0.5)))) +
  scale_x_continuous(limits = c(-20, 20)) +
  # annotate("text", x = -3, y = 80, size = 3,
  #          label = expression(paste("Strongest priors: ", alpha%~%N(0, 1),", ", ~ beta%~%N(0, 1),", ", ~ sigma%~%exp(0.5)))) +
  ylim(-80, 80) +
  theme(plot.subtitle = element_text(size = 6)) +
  NULL


# sim_reglines_all <- plot_grid(sim_reglines_a, sim_reglines_b, sim_reglines_c, ncol = 1, align = "v")




# Simulate data sets ------------------------------------------------------

sim_data_a <- fake_data %>% 
  filter(response != "musims" & model == "a") %>%
  filter(sim <= 1) %>% 
  mutate(mean = mean(value),
         sim = paste0("Simulation ", sim)) %>% 
  ggplot(aes(x = log_pred_mass, y = value)) + 
  geom_point(size = 0.1, alpha = 0.2) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  geom_text_repel(data = reference_points %>% filter(model == "a"),
            aes(x = max(d$log_pred) + 2,
                y = y, label = reference,
                color = reference),
            hjust = 1, 
            size = 3, 
            nudge_x = 2,
            nudge_y = 15,
            direction = "y") +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Simulated prey mass log(g)",
       x = "Mass of predator log(g)",
       title = expression(paste("One prior simulation of data (",y["i"], " ~ N(", mu["i"],",",sigma,"))")),
       subtitle = expression(paste("Weak priors: ", alpha%~%N(0, 1000),", ", ~ beta%~%N(0, 1000),", ", ~ sigma%~%exp(0.0001)))) +
  scale_x_continuous(limits = c(-20, 20)) +
  ylim(-60000, 60000) +
  theme(plot.subtitle = element_text(size = 6),
        plot.title = element_text(size = 8)) +
  # facet_grid(. ~ sim) +
  NULL


sim_data_b <- fake_data %>% 
  filter(response != "musims" & model == "b") %>%
  filter(sim <= 1) %>% 
  mutate(mean = mean(value),
         sim = paste0("Simulation ", sim)) %>% 
  ggplot(aes(x = log_pred_mass, y = value)) + 
  geom_point(size = 0.1, alpha = 0.2) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  geom_text(data = reference_points %>% filter(model == "c"),
            aes(x = max(d$log_pred) + 2,
                y = y, label = reference,
                color = reference),
            hjust = 0, 
            size = 3, 
            nudge_x = -0.2,
            nudge_y = 15) +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Simulated prey mass log(g)",
       x = "Mass of predator log(g)",
       title = "",
       subtitle = expression(paste("Stronger priors: ", alpha%~%N(0, 10),", ", ~ beta%~%N(0, 10),", ", ~ sigma%~%exp(0.01)))) +
  scale_x_continuous(limits = c(-20, 20)) +
  ylim(-1000, 1000) +
  theme(plot.subtitle = element_text(size = 6)) +
  # facet_grid(. ~ sim) +
  NULL

sim_data_c <- fake_data %>% 
  filter(response != "musims" & model == "c") %>%
  filter(sim <= 1) %>% 
  mutate(mean = mean(value),
         sim = paste0("Simulation ", sim)) %>% 
  ggplot(aes(x = log_pred_mass, y = value)) + 
  geom_point(size = 0.1, alpha = 0.2) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  geom_text(data = reference_points %>% filter(model == "c"),
            aes(x = max(d$log_pred) + 2,
                y = y, label = reference,
                color = reference),
            hjust = 0, 
            size = 3, 
            nudge_x = -0.2,
            nudge_y = 5) +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Simulated prey mass log(g)",
       x = "Mass of predator log(g)",
       title = "",
       subtitle = expression(paste("Strongest priors: ", alpha%~%N(0, 1),", ", ~ beta%~%N(0, 1),", ", ~ sigma%~%exp(0.5)))) +
  scale_x_continuous(limits = c(-20, 20)) +
  # annotate("text", x = -3, y = 80, size = 3,
  #          label = expression(paste("Strongest priors: ", alpha%~%N(0, 1),", ", ~ beta%~%N(0, 1),", ", ~ sigma%~%exp(0.1)))) +
  ylim(-80, 80) +
  theme(plot.subtitle = element_text(size = 6)) +
  # facet_grid(. ~ sim) +
  NULL


sim_all <- plot_grid(sim_reglines_a, 
                     sim_reglines_b,
                     sim_reglines_c,
                     sim_data_a, 
                     sim_data_b, 
                     sim_data_c, ncol = 3, align = "v",
                     labels = "auto")


# sim_all <- readRDS("plots/mod_1.rds")
saveRDS(sim_all, file = "plots/mod_1.rds")
ggsave(sim_all, file = "plots/mod_1.tiff", dpi = 400, width = 12, height = 8)
ggsave(sim_all, file = "plots/mod_1.jpg", dpi = 400, width = 9, height = 6)



# Plot prior v post -------------------------------------------------------
a <- readRDS(here("models/weak_brm_d.rds"))
b <- readRDS(here("models/stronger_brm_d.rds"))
c <- readRDS(here("models/strongest_brm_d.rds"))

c_cond <- conditional_effects(c)
c_cond_plot <- tibble(c_cond$log_pred) %>% clean_names()

post_pred_prey <- c_cond_plot %>% 
  ggplot(aes(x = log_pred, y = estimate)) + 
  theme_classic() +
  geom_ribbon(aes(x = log_pred, ymax = upper, ymin = lower), alpha = 0.5) +
  geom_line() +
  labs(y = "Mass of prey log(g)",
       x = "Mass of predator log(g)") +
  geom_point(data = d, aes(x = log_pred, y = log_prey), size = 0.2) +
  geom_line(data = fake_data %>% 
              filter(response == "musims" & model == "c") %>%
              group_by(sim) %>% 
              slice(seq(1, n(), by = 25)),
            size = 0.2, aes(group = sim, x = log_pred_mass, y = value), alpha = 0.1) +
  ylim(-20, 10) +
  NULL

saveRDS(post_pred_prey, file = here("plots/post_pred_prey.rds"))
ggsave(post_pred_prey, file = here("plots/post_pred_prey.tiff"), dpi = 500, width = 5, height = 5)
ggsave(post_pred_prey, file = here("plots/post_pred_prey.jpg"), dpi = 500, width = 5, height = 5)



