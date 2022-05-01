library(tidyverse)
library(readr)
library(scales)

data <- read_csv("<<PATH TO FILE WITH EXTRACTED CASES, DEATH, RECOVERIES>>")
updated_cases <- read_csv("<<PATH TO FILE WITH LAST CASES>>") %>%
  filter(date >= as.Date("2021-04-01")) %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012), 11000),
         IdLandkreis = replace(IdLandkreis, IdLandkreis == 16056, 16063)) %>%
  group_by(date, IdLandkreis) %>%
  summarise(AnzahlFall = sum(AnzahlFall)) %>%
  ungroup()


data <- data %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012), 11000),
         IdLandkreis = replace(IdLandkreis, IdLandkreis == 16056, 16063)) %>%
  group_by(Meldedatum, IdLandkreis) %>%
  summarise(AnzahlFall = sum(AnzahlFall),
            AnzahlTodesfall = sum(AnzahlTodesfall),
            AnzahlGenesen = sum(AnzahlGenesen)) %>%
  ungroup()

# RKI is missing some files in April <- might actually make sense to switch to the ARD data at some point
data2 <- updated_cases %>%
  left_join(data, by = c("date" = "Meldedatum", "IdLandkreis" = "IdLandkreis")) %>%
  mutate(active_cases2 = AnzahlFall.x - AnzahlTodesfall - AnzahlGenesen,
         diff = AnzahlFall.x - AnzahlFall.y) %>%
  rename(Meldedatum = date) %>%
  filter(complete.cases(.))

data <- data %>%
  mutate(active_cases = AnzahlFall - AnzahlTodesfall - AnzahlGenesen)

total_active_cases <- data %>%
  group_by(Meldedatum) %>%
  summarise(active_cases = sum(active_cases),
            deaths = sum(AnzahlTodesfall),
            recoveries = sum(AnzahlGenesen),
            cases = sum(AnzahlFall))

total_active_cases_prop <- total_active_cases %>%
  mutate(active_cases_prop = active_cases / total_population,
         deaths_prop = deaths / total_population,
         recoveries_prop = recoveries / total_population)

ggplot(total_active_cases, aes(x = Meldedatum, y = active_cases)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(total_active_cases, aes(x = Meldedatum, y = cases)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(total_active_cases, aes(x = Meldedatum, y = deaths)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(total_active_cases, aes(x = Meldedatum, y = recoveries)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 


ggplot(data, aes(x = Meldedatum, y = active_cases, color = IdLandkreis, group = IdLandkreis)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(data, aes(x = Meldedatum, y = AnzahlTodesfall, color = IdLandkreis, group = IdLandkreis)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(data, aes(x = Meldedatum, y = AnzahlGenesen, color = IdLandkreis, group = IdLandkreis)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(data, aes(x = Meldedatum, y = AnzahlFall, color = IdLandkreis, group = IdLandkreis)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

# normalize data by population
# where do I get data from 2020? - don't want to read every single PDF

county_data <- read_csv("<<PATH TO COUNTY DATA>>")

data_county_prop <- data %>%
  left_join(county_data, by = c("IdLandkreis" = "lk_id")) %>%
  select(IdLandkreis, Meldedatum, AnzahlFall, AnzahlTodesfall, AnzahlGenesen, active_cases, lk, bundesland, population) %>%
  mutate(active_cases_prop = (AnzahlFall - AnzahlTodesfall - AnzahlGenesen) / population,
         deaths_prop = AnzahlTodesfall / population,
         recoveries_prop = AnzahlGenesen / population,
         cases_prop = AnzahlFall / population)

write_csv(data_county_prop, "<<OUTPUT PATH COUNTY CASE PROP RKI>>")

total_population <- sum(county_data$population)

ggplot(total_active_cases_prop, aes(x = Meldedatum, y = active_cases_prop)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(total_active_cases_prop, aes(x = Meldedatum, y = recoveries_prop)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 



ggplot(data_county_prop, aes(x = Meldedatum, y = active_cases_prop, color = bundesland, group = IdLandkreis)) +
  geom_line(alpha=0.5) +
  scale_y_continuous(labels = comma) +
  ylim(0, 0.045) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(data_county_prop, aes(x = Meldedatum, y = deaths_prop, color = IdLandkreis, group = IdLandkreis)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(data_county_prop, aes(x = Meldedatum, y = recoveries_prop, color = IdLandkreis, group = IdLandkreis)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(data_county_prop, aes(x = Meldedatum, y = cases_prop, color = IdLandkreis, group = IdLandkreis)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

# still have to sum up wartburgkreis and x
# need to find the populations for Berlin (Probably best to just merge for now)


est_data <- read_csv("<<PATH TO ESTIMATED ACTIVE CASES WITH 14-day recovery time>>")

est_data <- est_data %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012), 11000),
         IdLandkreis = replace(IdLandkreis, IdLandkreis == 16056, 16063)) %>%
  group_by(date, IdLandkreis) %>%
  summarise(AnzahlFall = sum(AnzahlFall)) %>%
  ungroup()
  
# TODO when refactoring don't forget empty cases

est_data_norm <- est_data %>%
  left_join(county_data, by = c("IdLandkreis" = "lk_id")) %>%
  select(date, IdLandkreis, AnzahlFall, lk_movement, bundesland, population) %>%
  mutate(active_case_prop = AnzahlFall / population) %>%
  filter(date >= as.Date("2020-01-22"))

write_csv(est_data_norm, "<<OUTPUT PATH>>")

ggplot(est_data, aes(x = date, y = AnzahlFall, color = IdLandkreis, group = IdLandkreis)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(est_data_norm, aes(x = date, y = active_case_prop, color = IdLandkreis, group = IdLandkreis)) +
  geom_line(alpha=0.5) +
  scale_y_continuous(labels = comma) +
  ylim(0, 0.045) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 


est_active_cases_prop <- est_data %>%
  group_by(date) %>%
  summarise(total_active_cases = sum(AnzahlFall)) %>%
  mutate(total_active_cases_prop = total_active_cases / total_population)

ggplot() +
  geom_line(data = est_active_cases_prop, aes(x = date, y = total_active_cases)) +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 
  
# differeneces rki - estimate
y <- est_data_norm %>%
  inner_join(data_county_prop, by = c("date" = "Meldedatum", "IdLandkreis" = "IdLandkreis")) %>%
  select(date, IdLandkreis, active_case_prop, active_cases_prop, AnzahlFall.x, active_cases) %>%
  mutate(diff_prop = active_cases_prop - active_case_prop,
         diff_abs = active_cases - AnzahlFall.x)

y %>%
  select(diff_prop, diff_abs) %>%
  summary()

sd(y$diff_prop, na.rm = T)
sd(y$diff_abs, na.rm = T)

y <- y %>%
  filter(complete.cases(.))

cor(y$active_cases_prop, y$active_case_prop)

ggplot(y, aes(x=active_cases_prop, y=active_case_prop)) +
  geom_hex(bins=100) +
  xlab(label = "'Real' active cases (no updates)") +
  ylab(label = "Estimated active cases (with updates)") +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 



ggplot(y, aes(x=diff_prop)) +
  geom_histogram(binwidth = 0.00025) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(y, aes(x=diff_abs)) +
  geom_histogram(binwidth = 100)
# 2G compartment

ggplot() +
  geom_line(data = total_active_cases_prop, aes(x = Meldedatum, y = active_cases_prop), color="red") +
  geom_line(data = est_active_cases_prop, aes(x = date, y = total_active_cases_prop)) +
  scale_y_continuous(labels = comma) +
  ylab(label = "Proportion of active cases") +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

# change this code in a way that i can just insert different files
# wrap plotting in functions that automatically save to plots 
# Need to be careful with movement data -> already contains effects of the lockdowns -> look at mean travel and variation ()

# Try and model the waves <- via events
# what can we explain without modelling vaccinated people
# Estimate active cases - based on 14 days of recovery
# movement from abroad
# -> don' model numbers abroad -> retrieve number of countries 
# prop of infected + travel flux 

# How does travel from and to other countries influence the dynamics
# How does reduced travel in general influence the dynamics 
