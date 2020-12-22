library(tidyverse)
library(ggthemes)
library(rethinking)
library(cowplot)
library(here)
library(brms)
library(readr)
library(ggrepel)
library(janitor)
library(viridis)

# read data from Brose et al. 2006
d <- read.csv(here("data/pred-prey-mass.csv"))

# make log prey and predator
d$log_pred <- log(d$pred)
d$log_prey <- log(d$prey)

# Simulate outcomes from different prior predictive distributions (a/b/sig1, a2/b2/sig2, etc)
set.seed(1095)
N <- 100 # number of simulations
fake_data <- tibble(a = rnorm(N, 0, 1000), 
                    b = rnorm(N, 0, 1000),
                    a2 = rnorm(N, 0, 10),
                    b2 = rnorm(N, 0, 10),
                    a3 = rnorm(N, 0, 1),
                    b3 = rnorm(N, 0, 1),
                    sig1 = 0.0001,
                    sig2 = 0.01,
                    sig3 = 0.5,
                    sim = 1:N) %>%  
  expand_grid(log_pred_mass = d$log_pred) %>% 
  mutate(musims_1 = a + b*log_pred_mass, # simulate means
         logpreymasssims_1 = musims_1 + rnorm(nrow(.), 0, rexp(nrow(.), min(sig1))), # simulate new data
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

# reference points to plot
reference_points <- tibble(reference = c("Whale", "Virus", "Earth", "Atom"),
                           y = c(18.18, -32.2, 63, -52)) %>% # log grams of each reference point (from Wikipedia or Google)
  mutate(response = "logpreymassims") %>% 
  expand_grid(model = c("a", "b","c")) 

saveRDS(fake_data, file = here("data/fake_data_predator_prey.rds"))



# Fit model with strongest priors -----------------------------------------
# This step is done here only to facilitate making the plots in the main text. Otherwise, this step would wait
# until after choosing priors.

strongest_brm_d <- brm(log_prey ~ log_pred, data = d,
                       family = gaussian(),
                       prior = c(prior(normal(0, 1), class = "Intercept"),
                                 prior(normal(0, 1), class = "b"),
                                 prior(exponential(.5), class = "sigma")))

saveRDS(strongest_brm_d, file = here("models/strongest_brm_d.rds"))

# Extract posterior samples
posts_strongest <- posterior_samples(strongest_brm_d) %>% as_tibble() %>% clean_names() %>% mutate(priors = "strongest")


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
                  aes(x = max(d$log_pred) + 2,
                      y = y, 
                      label = reference,
                      color = reference),
                  hjust = 1, 
                  size = 5, 
                  nudge_x = 2,
                  nudge_y = 15,
                  direction = "y") +
  guides(color = F) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_classic() +
  # scale_x_continuous(limits = c(-7.8, -6.1)) +
  labs(y = "Prey Mass log(g)",
       x = "Predator Mass log(g)",
       title = expression(paste("Simulated Means (",mu["i"], " = ", alpha, " + ", beta, "x"["i"],")")),
       subtitle = "Weak Priors") +
  scale_x_continuous(limits = c(-20, 20)) +
  # annotate("text", x = -3, y = 60000, size = 5,
  #          label = expression(paste("Weak priors: ", alpha%~%N(0, 1000),", ", ~ beta%~%N(0, 1000),", ", ~ sigma%~%exp(0.0001)))) +
  ylim(-60000, 60000) +
  theme(text = element_text(size = 16),
        legend.title = element_blank()) +
  NULL



sim_reglines_b <- fake_data %>% 
  filter(response == "musims" & model == "b") %>%
  group_by(sim) %>% 
  slice(seq(1, n(), by = 25)) %>% 
  mutate(mean = mean(value)) %>% 
  ggplot(aes(x = log_pred_mass, y = value)) + 
  geom_line(size = 0.2, aes(group = sim), alpha = 0.4) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y, color = reference)) +
  geom_text_repel(data = reference_points %>% filter(model == "a"),
                  aes(x = max(d$log_pred) + 2,
                      y = y, label = reference,
                      color = reference),
                  hjust = -1, 
                  size = 5, 
                  nudge_x = 2,
                  nudge_y = 15,
                  direction = "y") +
  guides(color = F) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Prey Mass log(g)",
       x = "Predator Mass log(g)",
       title = "",
       subtitle = "Stronger Priors") +
  scale_x_continuous(limits = c(-20, 20)) +
  # annotate("text", x = -3, y = 3000, size = 5,
  #          label = expression(paste("Stronger priors: ", alpha%~%N(0, 10),", ", ~ beta%~%N(0, 10),", ", ~ sigma%~%exp(0.01)))) +
  ylim(-1000, 1000) +
  theme(text = element_text(size = 16),
        legend.title = element_blank()) +
  NULL



# read in posteriors from model fit above
c <- readRDS(here("models/strongest_brm_d.rds"))
c_cond <- conditional_effects(c)
c_cond_plot <- tibble(c_cond$log_pred) %>% clean_names()


sim_reglines_c <- fake_data %>% 
  filter(response == "musims" & model == "c") %>%
  group_by(sim) %>% 
  slice(seq(1, n(), by = 25)) %>% 
  mutate(mean = mean(value)) %>% 
  ggplot(aes(x = log_pred_mass)) + 
  geom_line(size = 0.2, aes(group = sim, y = value), alpha = 0.8) +
  geom_abline(data = reference_points, aes(slope = 0, intercept = y,
                                           color = reference)) +
  geom_text(data = reference_points %>% filter(model == "c"),
          aes(x = max(d$log_pred) + 2,
              y = y, label = reference,
              color = reference),
          hjust = 0, 
          size = 5, 
          nudge_x = -0.2,
          nudge_y = 5) +
  guides(color = F) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Prey Mass log(g)",
       x = "Predator Mass log(g)",
       title = "",
       subtitle = "Strongest Priors (with Posterior)") +
  scale_x_continuous(limits = c(-20, 20)) +
  # annotate("text", x = -3, y = 80, size = 5,
  #          label = expression(paste("Strongest priors: ", alpha%~%N(0, 1),", ", ~ beta%~%N(0, 1),", ", ~ sigma%~%exp(0.5)))) +
  ylim(-80, 80) +
  theme(text = element_text(size = 16),
        legend.title = element_blank()) +
  geom_ribbon(data = c_cond_plot, aes(x = log_pred, ymin = lower, ymax = upper), color = "#E69F00", size = 1.2) +
  NULL



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
            size = 5, 
            nudge_x = 2,
            nudge_y = 15,
            direction = "y") +
  guides(color = F) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Prey Mass log(g)",
       x = "Predator Mass log(g)",
       title = expression(paste("Simulated Data (n=1) (",y["i"], " ~ N(", mu["i"],",",sigma,"))")),
       subtitle = "Weak Priors") +
  scale_x_continuous(limits = c(-20, 20)) +
  ylim(-60000, 60000) +
  theme(text = element_text(size = 16),
        legend.title = element_blank()) +
  NULL


sim_data_b <- fake_data %>% 
  filter(response != "musims" & model == "b") %>%
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
                  hjust = -1, 
                  size = 5, 
                  nudge_x = 2,
                  nudge_y = 15,
                  direction = "y") +
  guides(color = F) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Prey Mass log(g)",
       x = "Predator Mass log(g)",
       title = "",
       subtitle = "Stronger Priors") +
  scale_x_continuous(limits = c(-20, 20)) +
  ylim(-1000, 1000) +
  theme(text = element_text(size = 16),
        legend.title = element_blank()) +
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
            size = 5, 
            nudge_x = -0.2,
            nudge_y = 5) +
  guides(color = F) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  labs(y = "Prey Mass log(g)",
       x = "Predator Mass log(g)",
       title = "",
       subtitle = "Strongest Priors (with raw data)") +
  scale_x_continuous(limits = c(-20, 20)) +
  # annotate("text", x = -3, y = 80, size = 5,
  #          label = expression(paste("Strongest priors: ", alpha%~%N(0, 1),", ", ~ beta%~%N(0, 1),", ", ~ sigma%~%exp(0.1)))) +
  ylim(-80, 80) +
  theme(text = element_text(size = 16),
        legend.title = element_blank()) +
  geom_point(data = d, aes(x = log_pred, y = log_prey), color = "#E69F00", shape = 21, size = 0.1) + # add real data
  NULL


# combine panels
sim_all <- plot_grid(sim_reglines_a, 
                     sim_reglines_b,
                     sim_reglines_c,
                     sim_data_a, 
                     sim_data_b, 
                     sim_data_c, ncol = 3, align = "v",
                     labels = "auto")

saveRDS(sim_all, file = "plots/mod_1.rds")
ggsave(sim_all, file = "plots/mod_1.tiff", dpi = 400, width = 16, height = 11)
ggsave(sim_all, file = "plots/mod_1.jpg", dpi = 400, width = 16, height = 11)



