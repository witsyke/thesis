library(tidyverse)

sir_experiment_output_path <- "<<PATH TO THE FULL SIMULATION OUTPUT>>"
R0_output_path <- "<<PATH TO THE FULL R0 OUTPUT>>"

fit <- "fit"
beta_type <- "fit"
gamma_type <- "fit"
lockdown = tibble(type = c("Partial lockdown", "Lockdown"), time = c(7*7, 13*7))


# Load results of the SIR simulation for all scenarios and combine them with R0 over time for the scenarios
for(experiment in c("observed", "no_mobility", "only_partial", "partial_from_beginning", "extended_at_partial", "extended_no_partial", "extended_from_beginning")){
  print(experiment)
  
  
  s_ij <- read_csv(paste(sir_experiment_output_path, experiment, fit, "long_all.csv", sep = "_")) %>%
    filter(grepl("s_", variable, fixed = T)) %>%
    mutate(start_dest = substring(variable, 3)) %>%
    select(start_dest, time, s_ij = value)
  
  R0 <- read_csv(paste(R0_output_path, experiment, beta_type, gamma_type, "2.csv", sep = "_")) %>% 
    mutate(start_dest = paste(start, dest, sep = "&")) %>%
    select(start_dest, time, R0_ij, R0_ij_weighted, pop)
  
  R0_s_ij <- R0 %>%
    left_join(s_ij, by = c("start_dest", "time"))
  
  
  R_eff_s_ij <- R0_s_ij %>% 
    mutate(R_eff_ij = R0_ij * s_ij,
           R_eff_ij_weighted = R_eff_ij * pop) %>%
    group_by(time) %>%
    summarise(R_eff_country = sum(R_eff_ij_weighted) / sum(pop)) 
  
  # Save R_eff over time in case plots need to be updated
  write_csv(R_eff_s_ij, file = paste(R0_output_path, experiment, fit, "2country.csv", sep = "_"))
  
  
  # Plot the time series
  ggplot(R_eff_s_ij, aes(x=time, y=R_eff_country, group=1)) +
    geom_line() + 
    geom_vline(data = lockdown, aes(xintercept=time, linetype=type)) + 
    geom_hline(yintercept=1, color="red") + 
    ylab("Basic reproduction number") +
    xlab("Time t") +
    scale_y_continuous(limits = c(0, 2.25)) +
    theme_bw() +
    theme(axis.title=element_text(size=22,face="bold"),
          axis.text = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_blank(),
          legend.position = c(0.95, 0.95))
}

# ---------------- PLOT R_eff ------------------

observed <- read_csv(paste(paste("R0_output_path", "R_eff_observed"), 
                           fit, "2country.csv", sep = "_"))  %>%
  mutate(experiment = "Observed mobility",
         experiment_id = "0")

no_lockdown <- read_csv(paste(paste("R0_output_path", "R_eff_no_mobility"), 
                              fit, "2country.csv", sep = "_")) %>%
  mutate(experiment = "No mobility change",
         experiment_id = "1")

partial_lockdown_only <- read_csv(paste(paste("R0_output_path", "R_eff_only_partial"), 
                                        fit, "2country.csv", sep = "_"))  %>%
  mutate(experiment = "Only partial lockdown mobility",
         experiment_id = "2")

partial_lockdown_start <- read_csv(paste(paste("R0_output_path", "R_eff_partial_from_beginning"), 
                                         fit, "2country.csv", sep = "_")) %>%
  mutate(experiment = "Partial lockdown mobility from the beginning",
         experiment_id = "3") 

full_at_partial_lockdown <- read_csv(paste(paste("R0_output_path", "R_eff_extended_at_partial"), 
                                           fit, "2country.csv", sep = "_")) %>%
  mutate(experiment = "Extended lockdown mobility at the partial lockdown",
         experiment_id = "4") 


full_lockdown_only <- read_csv(paste(paste("R0_output_path", "R_eff_extended_no_partial"), 
                                     fit, "2country.csv", sep = "_"))  %>%
  mutate(experiment = "Only extended lockdown mobility",
         experiment_id = "5")

full_lockdown_start <- read_csv(paste(paste("R0_output_path", "R_eff_extended_from_beginning"), 
                                      fit, "2country.csv", sep = "_")) %>%
  mutate(experiment = "Extended lockdown mobility from the beginning",
         experiment_id = "6")


combined <- rbind(observed, 
                  no_lockdown, 
                  full_lockdown_start, 
                  partial_lockdown_start,
                  partial_lockdown_only,
                  full_lockdown_only,
                  full_at_partial_lockdown) %>%
  mutate(experiment = factor(experiment, 
                             levels = c("Observed mobility", 
                                        "No mobility change", 
                                        "Only partial lockdown mobility", 
                                        "Partial lockdown mobility from the beginning",
                                        "Extended lockdown mobility at the partial lockdown",
                                        "Only extended lockdown mobility",
                                        "Extended lockdown mobility from the beginning")))

write_csv(combined, file = paste(R0_output_path, "R_eff_combined2.csv"))

ggplot(combined, aes(x = time, y = R_eff_country, color = experiment)) +
  geom_vline(data = lockdown, aes(xintercept=time, linetype=type), size = 1) + 
  geom_hline(yintercept=1, color="red", linetype="longdash", size = 1) + 
  geom_line(size = 1) +
  theme_bw() +
  xlab("Time t (in days)") +
  ylab("Effective reproduction number") +
  scale_y_continuous(limits = c(0, 2.25)) +
  scale_colour_manual(values = c("#00C094", "#F8766D", "#00B6EB", "#53B400", "#A58AFF", "#C49A00", "#FB61D7")) + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1, nrow = 4)) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        # legend.key.size = unit(2, "line"),
        axis.title = element_text(size = 22, face="bold"),
        axis.text = element_text(size=20))
