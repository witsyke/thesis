library(tidyverse)

x <- read_csv("<<PATH TO FILE WITH NEW CASES>>") 

y <- x %>%
  filter(date >= as.Date("2020-02-24"),
         date <= as.Date("2020-12-31"))

ggplot(est_national_case_data_2 %>%
         filter(date >= as.Date("2020-09-01"),
                date <= as.Date("2020-12-31")), aes(x=date, y=infected, color=IdLandkreis, group = IdLandkreis)) + 
  geom_line()


# AGGREGATE BERLIN AND WARTBURGKREIS
est_national_case_data <- x %>%
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

est_national_case_data_2 <- lk_dates %>%
  left_join(est_national_case_data) %>%
  mutate(infected = ifelse(is.na(AnzahlFall), 0, AnzahlFall)) %>%
  select(-AnzahlFall)


start_date = "2020-02-24"
end_date = "2020-12-31"


active_cases_temp <- est_national_case_data_2 %>%
  left_join(county_data, by = c("IdLandkreis" = "lk_id")) %>%
  select(name = lk_movement, date, AnzahlFall = infected, population) %>%
  mutate(incidence = AnzahlFall / population * 100000) %>%
  select(-AnzahlFall) %>%
  filter(date >= as.Date(start_date),
         date <= as.Date(end_date),
         complete.cases(.)) %>%
  pivot_wider(name, names_from = date, values_from = incidence) %>%
  arrange(name) %>%
  rownames_to_column() %>%
  mutate(rowname = as.double(rowname) - 1)

active_cases_temp %>%
  filter(!complete.cases(.))



write_csv(active_cases_temp, "<<OUTPUT PATH FOR NORMALIZED NEW CASES>>")


# write_csv(est_data_norm, "<<OUPUT PATH>>")

# ----------------------------------------------------------------------

# TODO super dirtly but there is one more lk that I don't have movement for
# pop <- tibble(lk = rownames(P)) %>%
#   left_join(est_data_norm) %>%
#   select(lk, population) %>%
#   distinct()
# 
# write_csv(pop, "<<OUTPUT PATH>>")
# 
# ggplot(est_data, aes(x = date, y = infected, color = IdLandkreis, group = IdLandkreis)) +
#   geom_line() +
#   scale_y_continuous(labels = comma) +
#   theme(axis.title=element_text(size=18,face="bold"),
#         axis.text = element_text(size=16)) 
# 
# ggplot(est_data_norm, aes(x = date, y = infected, color = IdLandkreis, group = IdLandkreis)) +
#   geom_line(alpha=0.5) +
#   scale_y_continuous(labels = comma) +
#   ylim(0, 0.045) +
#   theme(axis.title=element_text(size=18,face="bold"),
#         axis.text = element_text(size=16)) 
# 
# 
# est_active_cases_prop <- est_data %>%
#   group_by(date) %>%
#   summarise(total_active_cases = sum(AnzahlFall)) %>%
#   mutate(total_active_cases_prop = total_active_cases / total_population)
# 
# ggplot() +
#   geom_line(data = est_active_cases_prop, aes(x = date, y = total_active_cases_prop)) +
#   scale_y_continuous(labels = comma) +
#   theme(axis.title=element_text(size=18,face="bold"),
#         axis.text = element_text(size=16)) 
