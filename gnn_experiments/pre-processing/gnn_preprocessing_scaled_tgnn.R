library(tidyverse) 

start_date = "2020-02-24"
end_date = "2020-12-31"

#-----------------------ADJACENCY LIST------------------------------------------------------------
load(file = "<<PATH TO TOTAL TRAVEL SCALED DATA>>")
load(file = "<<PATH TO STRUCTURE SCALED DATA>>")

edge_list_week_structure <- scaled_movements_structure %>%
  mutate(travellers = round(scaled_travellers_day, digits = 2))
         
edge_list_week_amount <- scaled_movements_total_travel %>%
  mutate(travellers = round(scaled_travellers_day, digits = 2))



date = seq(as.Date(start_date), as.Date(end_date), by="day")
dates = tibble(date)%>%
  rownames_to_column()

covid_txs_edgelist_timed_structure <- dates %>%
  mutate(week = strftime(date, format = "%V"),
         year = strftime(date, format = "%Y"),
         match = paste(year, week, sep = "-")) %>%
  mutate(match = if_else(year == 2021 & week == 53, "2020-53", match)) %>%
  select(date, week = match, idx = rowname) %>%
  left_join(edge_list_week_structure, by = c("week" = "week")) %>%
  filter(complete.cases(.)) %>%
  select(idx, date, from, to, travellers)


covid_txs_edgelist_timed_amount <- dates %>%
  mutate(week = strftime(date, format = "%V"),
         year = strftime(date, format = "%Y"),
         match = paste(year, week, sep = "-")) %>%
  mutate(match = if_else(year == 2021 & week == 53, "2020-53", match)) %>%
  select(date, week = match, idx = rowname) %>%
  left_join(edge_list_week_amount, by = c("week" = "week")) %>%
  filter(complete.cases(.)) %>%
  select(idx, date, from, to, travellers)


view(covid_txs_edgelist_timed %>%
       filter(date == as.Date("2020-04-25")))

for (i in 1:length(dates$rowname)){
  print(i)
  
  write_csv(covid_txs_edgelist_timed_structure %>%
              filter(idx == i) %>%
              select(from, to, travellers),
            paste("<<OUTPUT PATH STRUCTURE>>", "GER_",
                  date[i],
                  ".csv",
                  sep = ""),
            col_names = FALSE)
  
  write_csv(covid_txs_edgelist_timed_amount %>%
              filter(idx == i) %>%
              select(from, to, travellers), 
            paste("<<OUTPUT PATH SCALE>>", "GER_",
                  date[i],
                  ".csv", 
                  sep = ""),
            col_names = FALSE) 
} 



