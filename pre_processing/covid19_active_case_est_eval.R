library(tidyverse)
library(readr)
library(scales)
setwd("<<PATH TO THIS DIRECTORY>>")

county_data <- read_csv("../data/population_data.csv")
total_population <- sum(county_data$population)


data <- read_csv("<<PATH TO EXTRACTED CASES, DEATHS, RECOVERIES>>") %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012), 11000),
         IdLandkreis = replace(IdLandkreis, IdLandkreis == 16056, 16063)) %>%
  group_by(Meldedatum, IdLandkreis) %>%
  summarise(AnzahlFall = sum(AnzahlFall),
            AnzahlTodesfall = sum(AnzahlTodesfall),
            AnzahlGenesen = sum(AnzahlGenesen)) %>%
  ungroup() %>%
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



ggplot(total_active_cases_prop, aes(x = Meldedatum, y = active_cases_prop)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 



# still have to sum up wartburgkreis and x
# need to find the populations for Berlin (Probably best to just merge for now)


est_data <- read_csv("<<PATH TO FILE WITH ESTIMATED ACTIVE CASES based on 14-day recovery>>")

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


ggplot(est_data_norm, aes(x = date, y = active_case_prop, color = IdLandkreis, group = IdLandkreis)) +
  geom_line(alpha=0.5) +
  scale_y_continuous(labels = comma, limits = c(0, 0.045)) +
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

data_county_prop <- read_csv("<<PATH TO FILE WITH COUNTY CASE PROP RKI>>") %>%
  rename(rki_cases_prop = active_cases_prop,
         rki_cases = active_cases)

# differeneces rki - estimate
y <- est_data_norm %>%
  rename(uniform_rec_cases_prop = active_case_prop,
         uniform_rec_cases = AnzahlFall) %>%
  inner_join(data_county_prop, by = c("date" = "Meldedatum", "IdLandkreis" = "IdLandkreis")) %>%
  select(date, IdLandkreis, uniform_rec_cases_prop, rki_cases_prop, uniform_rec_cases, rki_cases) %>%
  mutate(diff_prop = rki_cases_prop - uniform_rec_cases_prop,
         diff_abs = rki_cases - uniform_rec_cases,
         rki_cases_prop_100k = rki_cases_prop * 10^5,
         uniform_rec_cases_prop_100k = uniform_rec_cases_prop * 10^5,
         diff_100k = rki_cases_prop_100k - uniform_rec_cases_prop_100k) %>%
  filter(complete.cases(.))

y %>%
  select(diff_prop, diff_abs, diff_100k) %>%
  summary()

sd(y$diff_prop, na.rm = T)
sd(y$diff_abs, na.rm = T)
sd(y$diff_100k, na.rm = T)

y <- y 

cor(y$rki_cases_prop, y$uniform_rec_cases_prop)
cor(y$rki_cases_prop, y$uniform_rec_cases_prop, method = "spearman")
cor(y$rki_cases, y$uniform_rec_cases)
cor(y$rki_cases_prop_100k, y$uniform_rec_cases_prop_100k)

ggplot(y, aes(x=rki_cases_prop, y=uniform_rec_cases_prop)) +
  geom_hex(bins=100) +
  xlab(label = "Proportion of active cases at time t\n(RKI recoveries and deaths)") +
  ylab(label = "Proportion of active cases at time t\n(Uniform recovery time)") +
  labs(fill='Count') +
  theme_bw() +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16),
        legend.title = element_text(size=18,face="bold"),
        legend.text = element_text(size=16)) 

ggplot(y, aes(x=rki_cases, y=uniform_rec_cases)) +
  geom_hex(bins=100) +
  xlab(label = "Active cases at time t\n(RKI recoveries and deaths)") +
  ylab(label = "Active cases at time t\n(Uniform recovery time)") +
  labs(fill='Count') +
  theme_bw() +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16),
        legend.title = element_text(size=18,face="bold"),
        legend.text = element_text(size=16)) 

ggplot(y, aes(x=rki_cases_prop_100k, y=uniform_rec_cases_prop_100k)) +
  geom_hex(bins=100) +
  xlab(label = "Active cases per 100,000 at time t\n(RKI recoveries and deaths)") +
  ylab(label = "Active cases per 100,000 at time t\n(Uniform recovery time)") +
  labs(fill='Count') +
  theme_bw() +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16),
        legend.title = element_text(size=18,face="bold"),
        legend.text = element_text(size=16)) 



ggplot(y, aes(x=diff_prop)) +
  geom_histogram(binwidth = 0.00025) +
  xlab(label = "Difference in proportion of active cases") +
  ylab("Count") + 
  theme_bw() +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(y, aes(x=diff_abs)) +
  geom_histogram(binwidth = 100) +
  xlab(label = "Difference in active cases") +
  ylab("Count") + 
  theme_bw() +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(y, aes(x=diff_100k)) +
  geom_histogram(binwidth = 25) +
  xlab(label = "Difference in active cases") +
  ylab("Count") + 
  theme_bw() +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

# ggplot(y, aes(x=diff_abs)) +
#   geom_histogram(binwidth = 100)
# 2G compartment

compare_time_series_active_cases <- rbind(total_active_cases_prop %>%
  mutate(type = "RKI recoveries and deaths") %>%
  select(date = Meldedatum, cases = active_cases_prop, type),
  est_active_cases_prop %>%
    mutate(type = "Uniform recovery time") %>%
    select(date, cases = total_active_cases_prop, type))




ggplot() +
  geom_line(data = compare_time_series_active_cases, aes(x = date, y = cases, group = type, color = type), size = 1) +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  ylab(label = "Proportion of active cases") +
  xlab(label = "Month") +
  labs(color="Method") +
  theme(axis.title=element_text(size=22,face="bold"),
        axis.text = element_text(size=20),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.position = c(0.14, 0.93),
        plot.margin = margin(l=2, t=2, b=2, r = 20, unit = "pt"))
