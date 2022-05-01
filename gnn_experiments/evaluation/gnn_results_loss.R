tl <- read_csv("<<PATH TO TRAINING LOSS FILE>>", col_names = F) %>%
  rename(tl = X1)
vl <- read_csv("<<PATH TO TRAINING VALIDATION LOSS FILE>>", col_names = F) %>%
  rename(vl = X1)

test <- cbind(tl, vl) %>%
  rownames_to_column() %>%
  pivot_longer(-c("rowname")) %>%
  mutate(rowname = as.numeric(rowname))

ggplot(test, aes(x=rowname, y=value, color=name)) +
  geom_line()


setwd("<<PATH TO EXPERIMENT LOSS DIRECTORY>>")

file_names <- list.files(pattern = "*.csv")

# o = output.cpu().detach().numpy()
# l = y_test[0].cpu().numpy()
# --> o is prediction, l is real value

get_epoch_loss <- function(file_name){
  string <- str_split(file_name, "_")[[1]]
  type = string[1]
  time <- as.double(string[7]) # needs to be 7 and 8 for out only | 5 and 6 for mpnn
  shift <- as.double(str_split(string[8], ".csv")[[1]][1]) # little bit of a workaround to get the shift
  return(read_csv(file_name, col_names = F) %>%
           mutate(type = type,
                  time = time,
                  time_shift = time + shift,
                  shift = shift) %>%
           rownames_to_column() %>%
           select(epoch = rowname, type, loss = X1, time, time_shift, shift))
}

results <- file_names %>%
  map(get_epoch_loss)

results_combined <- Reduce(rbind, results)

ggplot(results_combined %>% mutate(epoch = as.numeric(epoch), shift_type = paste(type, time, shift, sep = "_")) %>% filter(time == 311, shift == 0), aes(x=epoch, y=loss, color=type, group=shift_type)) +
  geom_line()

results_combined %>% 
  group_by(type, time, sh)

results_combined %>%
  filter(type == "tain") %>%
  group_by(shift) %>%
  summarise(mean_loss = mean(loss))
