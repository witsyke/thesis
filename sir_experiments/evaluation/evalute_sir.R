library(tidyverse)
library(ggplot2)
library(ggExtra)

setwd("<<PATH TO THIS DIRECTORY>>")
national_pop <- read_csv("../data/national_pop_data.csv")

# set parametrization
parameters <-  "fit"

observed <- read_csv(paste("../results_epidemic_curve/experiment_observed", parameters, "long.csv", sep = "_")) %>%
  mutate(experiment = "Observed mobility",
         experiment_id = "0") 

no_lockdown <- read_csv(paste("../results_epidemic_curve/experiment_no_mobility", parameters, "long.csv", sep = "_")) %>%
  mutate(experiment = "No mobility change",
         experiment_id = "1")

partial_lockdown_only <- read_csv(paste("../results_epidemic_curve/experiment_only_partial", parameters, "long.csv", sep = "_")) %>%
  mutate(experiment = "Only partial lockdown mobility",
         experiment_id = "2")

partial_lockdown_start <- read_csv(paste("../results_epidemic_curve/experiment_partial_from_beginning", parameters, "long.csv", sep = "_")) %>%
  mutate(experiment = "Partial lockdown mobility from the beginning",
         experiment_id = "3")

full_at_partial_lockdown <- read_csv(paste("../results_epidemic_curve/experiment_extended_at_partial", parameters, "long.csv", sep = "_")) %>%
  mutate(experiment = "Extended lockdown mobility at the partial lockdown",
         experiment_id = "4")

full_lockdown_only <- read_csv(paste("../results_epidemic_curve/experiment_extended_no_partial", parameters, "long.csv", sep = "_")) %>%
  mutate(experiment = "Only extended lockdown mobility",
         experiment_id = "5")

full_lockdown_start <- read_csv(paste("../results_epidemic_curve/experiment_extended_from_beginning", parameters, "long.csv", sep = "_")) %>%
  mutate(experiment = "Extended lockdown mobility from the beginning",
         experiment_id = "6")



# -------------------- SCATTER PLOT VALUES --------------------------------------------

combined <- rbind(observed, 
                  no_lockdown, 
                  full_lockdown_start, 
                  partial_lockdown_start,
                  partial_lockdown_only,
                  full_lockdown_only,
                  full_at_partial_lockdown) %>%
  left_join(national_pop, by = c("county" = "lk")) %>%
  mutate(experiment = factor(experiment, 
                             levels = c("Observed mobility", 
                                        "No mobility change", 
                                        "Only partial lockdown mobility", 
                                        "Partial lockdown mobility from the beginning",
                                        "Extended lockdown mobility at the partial lockdown",
                                        "Only extended lockdown mobility",
                                        "Extended lockdown mobility from the beginning")))


combined_germany <- combined %>%
  mutate(active_cases = value * population) %>%
  group_by(experiment, time) %>%
  summarise(active_cases = sum(active_cases) / sum(population)) %>%
  ungroup()

lockdown = tibble(type = c("Partial lockdown", "Lockdown"), time = c(7*7, 13*7))

ggplot(combined_germany, aes(x = time, y = active_cases, color = experiment)) +
  geom_vline(data = lockdown, aes(xintercept=time, linetype=type)) + 
  geom_line(size = 1) +
  theme_bw() +
  xlab("Time t (in days)") +
  ylab("Infected j") +
  scale_x_continuous(limits = c(0, 395)) +
  scale_y_continuous(limits = c(0, 0.155)) +
  scale_colour_manual(values = c("#00C094", "#F8766D", "#00B6EB", "#53B400", "#A58AFF", "#C49A00", "#FB61D7")) + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1)) +
  theme(legend.position = c(0.77, 0.83),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        # legend.key.size = unit(2, "line"),
        axis.title = element_text(size = 22, face="bold"),
        axis.text = element_text(size=20))



combined_peaks <- combined %>%
  group_by(experiment, county) %>%
  filter(value == max(value)) %>%
  ungroup() 



value_vs_time <- ggplot(combined_peaks, aes(x = time, y = value, color = experiment)) +
  geom_vline(data = lockdown, aes(xintercept=time, linetype=type)) + 
  geom_point(size = 3, alpha = 0.5) +
  theme_bw() +
  xlab("Peak time t") +
  ylab("Peak infected j") + 
  scale_colour_manual(values = c("#00C094", "#F8766D", "#00B6EB", "#53B400", "#A58AFF", "#C49A00", "#FB61D7")) + 
  # scale_shape_manual(values = c(19, 17, 15, 3, 7, 8, 11)) +
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1, nrow = 4)) +
  scale_x_continuous(limits = c(0, 392)) +
  scale_y_log10() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        axis.title = element_text(size = 22, face="bold"),
        axis.text = element_text(size=20))

ggMarginal(value_vs_time, groupColour = TRUE, groupFill = TRUE)




# ---------------------------- SCATTER PLOT DIFFERENCES -------------------------------------
# join all experiments with observed then rbind in the end
observed_vs_no_lockdown <- observed %>%
  group_by(experiment, experiment_id, county) %>%
  filter(value == max(value)) %>%
  ungroup() %>%
  left_join(no_lockdown %>%
              group_by(experiment, experiment_id, county) %>%
              filter(value == max(value)) %>%
              ungroup(), by = c("county"), suffix = c(".observed", ".no_lockdown")) %>%
  mutate(difference_value = value.no_lockdown - value.observed,
         difference_time = time.no_lockdown - time.observed) %>%
  select(county, difference_value, difference_time, experiment = experiment.no_lockdown, experiment_id = experiment_id.no_lockdown)
  

observed_vs_full_lockdown_start <- observed %>%
  group_by(experiment, experiment_id, county) %>%
  filter(value == max(value)) %>%
  ungroup() %>%
  left_join(full_lockdown_start %>%
              group_by(experiment, experiment_id, county) %>%
              filter(value == max(value)) %>%
              ungroup(), by = c("county"), suffix = c(".observed", ".full_lockdown_start")) %>%
  mutate(difference_value = value.full_lockdown_start - value.observed,
         difference_time = time.full_lockdown_start - time.observed) %>%
  select(county, difference_value, difference_time, experiment = experiment.full_lockdown_start, experiment_id = experiment_id.full_lockdown_start)


observed_vs_partial_lockdown_start <- observed %>%
  group_by(experiment, experiment_id, county) %>%
  filter(value == max(value)) %>%
  ungroup() %>%
  left_join(partial_lockdown_start %>%
              group_by(experiment, experiment_id, county) %>%
              filter(value == max(value)) %>%
              ungroup(), by = c("county"), suffix = c(".observed", ".partial_lockdown_start")) %>%
  mutate(difference_value = value.partial_lockdown_start - value.observed,
         difference_time = time.partial_lockdown_start - time.observed) %>%
  select(county, difference_value, difference_time, experiment = experiment.partial_lockdown_start, experiment_id = experiment_id.partial_lockdown_start)

observed_vs_partial_lockdown_only <- observed %>%
  group_by(experiment, experiment_id, county) %>%
  filter(value == max(value)) %>%
  ungroup() %>%
  left_join(partial_lockdown_only %>%
              group_by(experiment, experiment_id, county) %>%
              filter(value == max(value)) %>%
              ungroup(), by = c("county"), suffix = c(".observed", ".partial_lockdown_only")) %>%
  mutate(difference_value = value.partial_lockdown_only - value.observed,
         difference_time = time.partial_lockdown_only - time.observed) %>%
  select(county, difference_value, difference_time, experiment = experiment.partial_lockdown_only, experiment_id = experiment_id.partial_lockdown_only)

observed_vs_full_lockdown_only<- observed %>%
  group_by(experiment, experiment_id, county) %>%
  filter(value == max(value)) %>%
  ungroup() %>%
  left_join(full_lockdown_only %>%
              group_by(experiment, experiment_id, county) %>%
              filter(value == max(value)) %>%
              ungroup(), by = c("county"), suffix = c(".observed", ".full_lockdown_only")) %>%
  mutate(difference_value = value.full_lockdown_only- value.observed,
         difference_time = time.full_lockdown_only - time.observed) %>%
  select(county, difference_value, difference_time, experiment = experiment.full_lockdown_only, experiment_id = experiment_id.full_lockdown_only)

observed_vs_full_at_partial_lockdown <- observed %>%
  group_by(experiment, experiment_id, county) %>%
  filter(value == max(value)) %>%
  ungroup() %>%
  left_join(full_at_partial_lockdown %>%
              group_by(experiment, experiment_id, county) %>%
              filter(value == max(value)) %>%
              ungroup(), by = c("county"), suffix = c(".observed", ".full_at_partial_lockdown")) %>%
  mutate(difference_value = value.full_at_partial_lockdown - value.observed,
         difference_time = time.full_at_partial_lockdown - time.observed) %>%
  select(county, difference_value, difference_time, experiment = experiment.full_at_partial_lockdown, experiment_id = experiment_id.full_at_partial_lockdown)

#------------------

combined_difference <- rbind(observed_vs_no_lockdown, 
                             observed_vs_full_lockdown_start,
                             observed_vs_partial_lockdown_start,
                             observed_vs_partial_lockdown_only,
                             observed_vs_full_lockdown_only,
                             observed_vs_full_at_partial_lockdown) %>%
  mutate(experiment = factor(experiment, 
                             levels = c("Observed mobility", 
                                        "No mobility change", 
                                        "Only partial lockdown mobility", 
                                        "Partial lockdown mobility from beginning",
                                        "Full lockdown mobility at partial lockdown",
                                        "Only lockdown mobility",
                                        "Lockdown mobility from beginning")))

# color scale for these plots is important as they need to have the color for observed removed to have the same color throughout
observed_vs_exper_scatter <- ggplot(combined_difference, aes(x = difference_time, y = difference_value, color = experiment), ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3, alpha = 0.6) +
  theme_bw() +
  xlim(-120, 260) +
  ylim(-0.01, 0.15) +
  xlab("Difference in peak time") +
  ylab("Difference in peak infected j") + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1, nrow = 3)) +
  scale_colour_manual(values = c("#F8766D", "#00B6EB", "#53B400", "#A58AFF", "#C49A00", "#FB61D7")) + 
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        axis.title = element_text(size = 22, face="bold"),
        axis.text = element_text(size=20))

ggMarginal(observed_vs_exper_scatter, groupColour = TRUE, groupFill = T)


# -------------------- BOXPLOT PEAK TIME DIFFERENCE -------------------------------------
ggplot(combined_difference, aes(x = experiment_id, y=difference_time, color=experiment)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_boxplot(size = 1) +
  theme_bw() +
  xlab("Experiment") +
  ylab("Difference in peak time") + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1, nrow = 3)) +
  scale_colour_manual(values = c("#F8766D", "#00B6EB", "#53B400", "#A58AFF", "#C49A00", "#FB61D7")) + 
  theme(
    legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=24),
        axis.title = element_text(size = 28, face="bold"),
        axis.text = element_text(size=24))

# -------------------- BOXPLOT PEAK VALUE DIFFERENCE -------------------------------------
ggplot(combined_difference, aes(x = experiment_id, y=difference_value, color=experiment)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_boxplot(size = 1) +
  theme_bw() +
  xlab("Experiment") +
  ylab("Difference in peak infected j") + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1, nrow = 3)) +
  scale_colour_manual(values = c("#F8766D", "#00B6EB", "#53B400", "#A58AFF", "#C49A00", "#FB61D7")) + 
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=24),
        axis.title = element_text(size = 28, face="bold"),
        axis.text = element_text(size=24))
