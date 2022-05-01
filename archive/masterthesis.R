library(tidyverse)
library(readr)

incidence_data <-read_csv("<<PATH TO LATEST RKI CASE FILE>>")
incidence_data <- incidence_data[rowSums(is.na(incidence_data)) != ncol(incidence_data),]
colnames(incidence_data[-(1:3)])

test <- incidence_data %>%
  filter(LKNR == 9162)

incidence_data_gathered <- incidence_data %>% 
  pivot_longer(colnames(incidence_data[-(1:3)]), names_to = "date", values_to = "incidence")



incidence_per_county <- incidence_data_gathered %>%
  select(-NR, -LKNR) %>%
  pivot_wider(names_from = "LK", values_from = "incidence")

result <- ccf(incidence_per_county["StadtRegion Aachen"], incidence_per_county["LK Böblingen"])
test <- cor(incidence_per_county %>% select(-date))

result$lag[which.max(result$acf)]

mapply(function(x, y) seq_len(x) + y,
       c(a =  1, b = 2, c = 3),  # names from first
       c(A = 10, B = 0, C = -10))

