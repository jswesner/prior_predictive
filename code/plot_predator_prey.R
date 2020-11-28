# set.seed for reproducibilty
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

sim_newdata <- fake_data %>% 
  filter(response == "logpreymasssims") %>% 
  filter(log_pred_mass == median(log_pred_mass)) %>%
  mutate(model = case_when(model == "a" ~ "d",
                           model == "b" ~ "e",
                           model == "c" ~ "f")) %>% 
  ggplot(aes(x = value, y = ..scaled..)) + 
  geom_line(aes(group = sim), stat="density", alpha=0.1) +
  facet_grid(.~model, scales = "free_x") +
  geom_vline(data = reference_points %>%
               mutate(model = case_when(model == "a" ~ "d",
                                        model == "b" ~ "e",
                                        model == "c" ~ "f")),
             aes(xintercept = y, color = reference)) +
  coord_cartesian(xlim = c(-250, 250),
                  ylim = c(0, 1.5)) +
  geom_text(data = reference_points %>%
              mutate(model = case_when(model == "a" ~ "d",
                                       model == "b" ~ "e",
                                       model == "c" ~ "f")), aes(x = y, y = 1.3, 
                                                                 label = reference, color = reference),
            angle = -90, vjust = -0.2, size = 3) +
  theme_classic() +
  scale_color_colorblind() +
  guides(color = F) +
  labs(x = "Predicted mass of prey (log grams) for an average sized predator", 
       title = "Simulated masses of individual prey") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())



sim_reglines <- fake_data %>% 
  filter(response == "musims") %>% 
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
  facet_grid(.~model, scales = "free_y") +
  geom_text(data = reference_points %>% filter(model == "c"),
            aes(x = max(d$log_pred),
                y = y, label = reference,
                color = reference),
            hjust = 0, 
            size = 2.5, 
            nudge_x = 0,
            nudge_y = 20) +
  guides(color = F) +
  scale_color_colorblind() +
  theme_classic() +
  # theme(plot.margin = unit(c(0, 5, 0, 0), "cm")) +
  coord_cartesian(xlim = c(-20, 20),
                  ylim = c(-300, 300)) +
  labs(y = "Mass of prey (log grams)",
       x = "Mass of predator (log grams)", 
       title = "Simulated regression lines") +
  NULL


mod_1 <- plot_grid(sim_reglines, sim_newdata, ncol = 1, align = "v")

saveRDS(mod_1, file = "plots/mod_1.rds")
ggsave(mod_1, file = "plots/mod_1.tiff", dpi = 400, width = 6, height = 6)
ggsave(mod_1, file = "plots/mod_1.jpg", dpi = 400, width = 6, height = 6)
