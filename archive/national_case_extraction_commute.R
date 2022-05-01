# TODO still need to refactor this ....
library(tidyverse)


est_national_case_data <- read_csv("<<PATH TO ACTIVE CASES>>")
est_national_recovered_data <- read_csv("<<PATH TO RECOVERIES>>")
county_data <- read_csv("<<PATH TO POPULATION DATA>>")



# AGGREGATE BERLIN AND WARTBURGKREIS
est_national_case_data <- est_national_case_data %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012), 11000),
         IdLandkreis = replace(IdLandkreis, IdLandkreis == 16056, 16063)) %>%
  group_by(date, IdLandkreis) %>%
  summarise(AnzahlFall = sum(AnzahlFall)) %>%
  ungroup()

est_national_recovered_data <- est_national_recovered_data %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012), 11000),
         IdLandkreis = replace(IdLandkreis, IdLandkreis == 16056, 16063)) %>%
  group_by(date, IdLandkreis) %>%
  summarise(AnzahlFall = sum(AnzahlFall)) %>%
  ungroup()


# TODO schwabach is missing from movement, so is weiden in der oberpfalz

date = seq(as.Date("2020-01-22"), as.Date("2021-12-01"), by="day")
dates = tibble(date)

lk_dates <- est_national_case_data %>%
  select(IdLandkreis) %>%
  distinct(IdLandkreis) %>%
  full_join(dates, by=character())

est_national_case_data <- lk_dates %>%
  left_join(est_national_case_data) %>%
  mutate(infected = ifelse(is.na(AnzahlFall), 0, AnzahlFall)) %>%
  select(-AnzahlFall)

est_national_recovered_data <- lk_dates %>%
  left_join(est_national_recovered_data) %>%
  mutate(recovered = AnzahlFall) %>%
  select(-AnzahlFall)


est_data_norm <- est_national_case_data %>%
  left_join(est_national_recovered_data, by = c("IdLandkreis", "date")) %>%
  left_join(county_data, by = c("IdLandkreis" = "lk_id")) %>%
  select(date, IdLandkreis, infected, recovered, lk_movement, bundesland, population) %>%
  filter(date >= as.Date("2020-01-22")) %>%
  select(date, lk = lk_movement, infected, recovered, population) %>%
  filter(complete.cases(.)) 
view(est_data_norm)

write_csv(est_data_norm, "<<OUTPUT PATH>>")

# TODO super dirtly but there is one more lk that I don't have movement for
pop <- tibble(lk = rownames(P)) %>%
  left_join(est_data_norm) %>%
  select(lk, population) %>%
  distinct()

write_csv(pop, "<<PATH TO SIR EXPERIMENT DATA>>")

ggplot(est_data, aes(x = date, y = infected, color = IdLandkreis, group = IdLandkreis)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 

ggplot(est_data_norm, aes(x = date, y = infected, color = IdLandkreis, group = IdLandkreis)) +
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
  geom_line(data = est_active_cases_prop, aes(x = date, y = total_active_cases_prop)) +
  scale_y_continuous(labels = comma) +
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) 
