library(tidyverse)

setwd("<<PATH TO MOVEMENT DIRECTORY>>")

load("results.Rdata")

results_complete <- results %>%
    filter(complete.cases(.))


mean_x <- mean(results_complete$active_case_prop)
ss_res <- sum((results_complete$active_case_prop - results_complete$value)^2)
ss_tot <- sum((results_complete$active_case_prop - mean_x)^2)

r_sqr_ <- 1 - ss_res / ss_tot
  
r_sqr <- cor(results_complete$active_case_prop, results_complete$value)^2

test <- results_complete %>%
    group_by(Compartment) %>%
    summarise(mean_x = mean(active_case_prop),
            ss_res = sum((active_case_prop - value)^2),
            ss_tot = sum((active_case_prop - mean_x)^2),
            corr = cor(active_case_prop, value)) %>%
    mutate(r_sqr = (1 - ss_res / ss_tot))


ggplot(test, aes(x = r_sqr)) +
    geom_histogram(bins = 1000)

test %>% 
    filter(r_sqr >= 0)

ggplot(test, (aes(y = r_sqr))) +
    geom_boxplot()

x <- results_complete %>%
    filter(Compartment == "j_Alb-Donau-Kreis")

sum(x$active_case_prop - mean(x$active_case_prop)^2)
