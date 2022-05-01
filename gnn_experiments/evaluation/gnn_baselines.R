# SCRIPT TO CALCULATE THE "PREDICTIONS" FOR THE NAIVE BASELINES USED FOR THE GNN EVALUATION
setwd("<<PATH TO SIR EXPERIMENTS>>")
# File with cases is reused
national_cases <- read_csv("data/est_norm_national_data.csv")


national_cases_date_plus_one <- national_cases %>%
  mutate(date = date + 1) # date + 1, so that when I join these two I have the information of the previous day

test <- national_cases %>%
  left_join(national_cases_date_plus_one, by = c("lk", "date"), suffix = c(".t", ".t-1")) %>%
  select(date, county = lk, infected.t, `infected.t-1`) %>%
  filter(date >= as.Date("2020-02-24") + 233,
         date <= as.Date("2020-02-24") + 310) %>%
  mutate(delta = infected.t - `infected.t-1`,
         `1` = infected.t + delta * 1,
         `2` = infected.t + delta * 2,
         `3` = infected.t + delta * 3,
         `4` = infected.t + delta * 4,
         `5` = infected.t + delta * 5,
         `6` = infected.t + delta * 6,
         `7` = infected.t + delta * 7,
         `8` = infected.t + delta * 8,
         `9` = infected.t + delta * 9,
         `10` = infected.t + delta * 10,
         `11` = infected.t + delta * 11,
         `12` = infected.t + delta * 12,
         `13` = infected.t + delta * 13,
         `14` = infected.t + delta * 14) 

prev_delta <- test %>%
  select(-c("infected.t", "infected.t-1", "delta")) %>%
  pivot_longer(-c(date, county), names_to = "shift", values_to = "prediction") %>%
  mutate(shift = as.numeric(shift),
         match_real = date + shift) %>%
  left_join(national_cases, by = c("match_real" = "date", "county" = "lk")) %>%
  select(date, county, shift, prediction, observed = infected) %>%
  mutate(absoulte_error = abs(prediction - observed)) %>%
  filter(!(date == as.Date("2020-12-30") & shift > 1),
         !(date == as.Date("2020-12-29") & shift > 2),
         !(date == as.Date("2020-12-28") & shift > 3),
         !(date == as.Date("2020-12-27") & shift > 4),
         !(date == as.Date("2020-12-26") & shift > 5),
         !(date == as.Date("2020-12-25") & shift > 6),
         !(date == as.Date("2020-12-24") & shift > 7),
         !(date == as.Date("2020-12-23") & shift > 8),
         !(date == as.Date("2020-12-22") & shift > 9),
         !(date == as.Date("2020-12-21") & shift > 10),
         !(date == as.Date("2020-12-20") & shift > 11),
         !(date == as.Date("2020-12-19") & shift > 12),
         !(date == as.Date("2020-12-18") & shift > 13))


prev_delta_abs_error_1 <- prev_delta %>%
  group_by(date, shift) %>%
  summarise(mean_absolute_error = mean(absoulte_error)) %>%
  ungroup()

prev_delta_abs_error_2 <- prev_delta_abs_error_1 %>%
  group_by(shift) %>%
  summarise(mean_absolute_error = mean(mean_absolute_error))

round((prev_delta_abs_error_2 %>%
         filter(shift <= 14) %>%
         summarise(mean = mean(mean_absolute_error)))$mean, digits = 2)


library(zoo)


test2 <- national_cases %>%
  left_join(national_cases_date_plus_one, by = c("lk", "date"), suffix = c(".t", ".t-1")) %>%
  select(date, county = lk, infected.t, `infected.t-1`) %>%
  mutate(delta = infected.t - `infected.t-1`) %>%
  group_by(county) %>%
  mutate(delta_07da = zoo::rollmean(delta, k = 7, fill = NA, align = "right")) %>%
  filter(date >= as.Date("2020-02-24") + 233,
         date <= as.Date("2020-02-24") + 310) %>%
  mutate(delta = infected.t - `infected.t-1`,
         `1` = infected.t + delta_07da * 1,
         `2` = infected.t + delta_07da * 2,
         `3` = infected.t + delta_07da * 3,
         `4` = infected.t + delta_07da * 4,
         `5` = infected.t + delta_07da * 5,
         `6` = infected.t + delta_07da * 6,
         `7` = infected.t + delta_07da * 7,
         `8` = infected.t + delta_07da * 8,
         `9` = infected.t + delta_07da * 9,
         `10` = infected.t + delta_07da * 10,
         `11` = infected.t + delta_07da * 11,
         `12` = infected.t + delta_07da * 12,
         `13` = infected.t + delta_07da * 13,
         `14` = infected.t + delta_07da * 14) 


prev_delta_07da <- test2 %>%
  select(-c("infected.t", "infected.t-1", "delta", "delta_07da")) %>%
  pivot_longer(-c(date, county), names_to = "shift", values_to = "prediction") %>%
  mutate(shift = as.numeric(shift),
         match_real = date + shift,
         prediction = round(prediction)) %>%
  left_join(national_cases, by = c("match_real" = "date", "county" = "lk")) %>%
  select(date, county, shift, prediction, observed = infected) %>%
  mutate(absoulte_error = abs(prediction - observed),
         squared_error = (prediction - observed)^2) %>%
  filter(!(date == as.Date("2020-12-30") & shift > 1),
         !(date == as.Date("2020-12-29") & shift > 2),
         !(date == as.Date("2020-12-28") & shift > 3),
         !(date == as.Date("2020-12-27") & shift > 4),
         !(date == as.Date("2020-12-26") & shift > 5),
         !(date == as.Date("2020-12-25") & shift > 6),
         !(date == as.Date("2020-12-24") & shift > 7),
         !(date == as.Date("2020-12-23") & shift > 8),
         !(date == as.Date("2020-12-22") & shift > 9),
         !(date == as.Date("2020-12-21") & shift > 10),
         !(date == as.Date("2020-12-20") & shift > 11),
         !(date == as.Date("2020-12-19") & shift > 12),
         !(date == as.Date("2020-12-18") & shift > 13)) 
# TODO this coudl theoretically be avoided if i filter national_cases before joining and then do a complete cases filter


prev_delta_07da_abs_error_1 <- prev_delta_07da %>%
  group_by(county, shift) %>%
  summarise(mean_absolute_error = mean(absoulte_error),
            mean_sqr_error = mean(squared_error)) %>%
  ungroup()

prev_delta_07da_abs_error_2 <- prev_delta_07da_abs_error_1 %>%
  group_by(shift) %>%
  summarise(mean_absolute_error = mean(mean_absolute_error),
            mean_sqr_error = mean(mean_sqr_error))

round((prev_delta_07da_abs_error_2 %>%
         filter(shift <= 14) %>%
         summarise(mean = mean(mean_absolute_error)))$mean, digits = 2)

# ---------------------------------------------------------------------
# This is the same file provided to the GNN to extract the dynamic vertex features
# This is used to ensure that the baselines and the NNs have the same underlying data
new_cases <- read_csv("<<PATH TO GNN VERTEX FEATURES>>") %>%
  pivot_longer(-c("rowname", "name"), names_to = "date", values_to = "cases") %>%
  select(date, county = name, cases) %>%
  mutate(date = as.Date(date))

test3 <- new_cases %>%
  group_by(county) %>%
  mutate(delta_07da = round(zoo::rollmean(cases, k = 7, fill = NA, align = "right"))) %>%
  filter(date >= as.Date("2020-02-24") + 189,
         date <= as.Date("2020-02-24") + 310) %>%
  mutate(`1` = delta_07da,
         `2` = delta_07da,
         `3` = delta_07da,
         `4` = delta_07da,
         `5` = delta_07da,
         `6` = delta_07da,
         `7` = delta_07da,
         `8` = delta_07da,
         `9` = delta_07da,
         `10` = delta_07da,
         `11` = delta_07da,
         `12` = delta_07da,
         `13` = delta_07da,
         `14` = delta_07da) 

cases_07da <- test3 %>%
  select(-c("cases", "delta_07da")) %>%
  pivot_longer(-c(date, county), names_to = "shift", values_to = "prediction") %>%
  mutate(shift = as.numeric(shift),
         match_real = date + shift) %>%
  left_join(new_cases, by = c("match_real" = "date", "county" = "county")) %>%
  select(date, county, shift, prediction, observed = cases) %>%
  mutate(absoulte_error = abs(prediction - observed),
         squared_error = (prediction - observed)^2) %>%
  filter(!(date == as.Date("2020-12-30") & shift > 1),
         !(date == as.Date("2020-12-29") & shift > 2),
         !(date == as.Date("2020-12-28") & shift > 3),
         !(date == as.Date("2020-12-27") & shift > 4),
         !(date == as.Date("2020-12-26") & shift > 5),
         !(date == as.Date("2020-12-25") & shift > 6),
         !(date == as.Date("2020-12-24") & shift > 7),
         !(date == as.Date("2020-12-23") & shift > 8),
         !(date == as.Date("2020-12-22") & shift > 9),
         !(date == as.Date("2020-12-21") & shift > 10),
         !(date == as.Date("2020-12-20") & shift > 11),
         !(date == as.Date("2020-12-19") & shift > 12),
         !(date == as.Date("2020-12-18") & shift > 13)) %>%
  mutate(shift = shift - 1,
         date = date + 1,
         experiment = "AVG_WEEK") %>%
  left_join(dates, by = c("date" = "value")) %>%
  left_join(county_data, by = c("county")) %>%
  select(experiment, county, real = observed, prediction, time = rowname, shift, population, date = date, start = date)

write_csv(cases_07da, file = paste("<<PATH TO AGGREGATED GNN RESULTS>>", "AVG_WEEK", ".csv", sep = ""))

cases_07da_abs_error_1 <- cases_07da %>%
  # filter(date >= as.Date("2020-10-15")) %>%
  group_by(county, shift) %>%
  summarise(mean_absolute_error = mean(absoulte_error),
            mean_sqr_error = mean(squared_error)) %>%
  ungroup()

cases_07da_abs_error_2 <- cases_07da_abs_error_1 %>%
  group_by(shift) %>%
  summarise(mean_absolute_error = mean(mean_absolute_error),
            mean_sqr_error = mean(mean_sqr_error))

round((cases_07da_abs_error_2 %>%
         filter(shift <= 3) %>%
         summarise(mean = mean(mean_absolute_error)))$mean, digits = 2)


# ----------------------------------------------------------------------------

test4 <- new_cases %>%
  group_by(county) %>%
  filter(date >= as.Date("2020-02-24") + 189,
         date <= as.Date("2020-02-24") + 310) %>%
  mutate(`1` = cases,
         `2` = cases,
         `3` = cases,
         `4` = cases,
         `5` = cases,
         `6` = cases,
         `7` = cases,
         `8` = cases,
         `9` = cases,
         `10` = cases,
         `11` = cases,
         `12` = cases,
         `13` = cases,
         `14` = cases) 

cases_ld <- test4 %>%
  select(-c("cases")) %>%
  pivot_longer(-c(date, county), names_to = "shift", values_to = "prediction") %>%
  mutate(shift = as.numeric(shift),
         match_real = date + shift) %>%
  left_join(new_cases, by = c("match_real" = "date", "county" = "county")) %>%
  select(date, county, shift, prediction, observed = cases) %>%
  mutate(absoulte_error = abs(prediction - observed),
         squared_error = (prediction - observed)^2) %>%
  filter(!(date == as.Date("2020-12-30") & shift > 1),
         !(date == as.Date("2020-12-29") & shift > 2),
         !(date == as.Date("2020-12-28") & shift > 3),
         !(date == as.Date("2020-12-27") & shift > 4),
         !(date == as.Date("2020-12-26") & shift > 5),
         !(date == as.Date("2020-12-25") & shift > 6),
         !(date == as.Date("2020-12-24") & shift > 7),
         !(date == as.Date("2020-12-23") & shift > 8),
         !(date == as.Date("2020-12-22") & shift > 9),
         !(date == as.Date("2020-12-21") & shift > 10),
         !(date == as.Date("2020-12-20") & shift > 11),
         !(date == as.Date("2020-12-19") & shift > 12),
         !(date == as.Date("2020-12-18") & shift > 13)) %>%
  mutate(shift = shift - 1,
         date = date + 1,
         experiment = "LAST_DAY") %>%
  left_join(dates, by = c("date" = "value")) %>%
  left_join(county_data, by = c("county")) %>%
  select(experiment, county, real = observed, prediction, time = rowname, shift, population, date = date, start = date)

write_csv(cases_ld, file = paste("<<PATH TO AGGREGATED GNN RESULTS>>", "LAST_DAY", ".csv", sep = ""))

ld_abs_error_1 <- cases_ld %>%
  # filter(date >= as.Date("2020-10-15")) %>%
  group_by(county, shift) %>%
  summarise(mean_absolute_error = mean(absoulte_error),
            mean_sqr_error = mean(squared_error)) %>%
  ungroup()

ld_abs_error_2 <- ld_abs_error_1 %>%
  group_by(shift) %>%
  summarise(mean_absolute_error = mean(mean_absolute_error),
            mean_sqr_error = mean(mean_sqr_error))

round((ld_abs_error_2 %>%
         filter(shift <= 14) %>%
         summarise(mean = mean(mean_absolute_error)))$mean, digits = 2)
