library(tidyverse)
library(ggplot2)
library(broom)
library(caret)
library(cvms)
library(scales)
setwd("<<PATH TO THIS DIRECTORY>>")

county_data <- read_csv("../../data/population_data.csv") %>%
  dplyr::select(county = lk_movement, population)

setwd("<<PATH TO AGGREGATED GNN RESULTS>>")
file_names <- list.files(pattern = "*.csv")


results <- file_names %>%
  map(read_csv) 

results_combined <- Reduce(rbind, results)

results_combined <- results_combined %>%
  mutate(real = if_else(experiment %in% c("LAST_DAY", "AVG_WEEK"), (real / population) * 10^5, real),
         prediction = if_else(experiment %in% c("LAST_DAY", "AVG_WEEK"), (prediction / population) * 10^5, prediction))




mean_absolute_error_county_shift <- results_combined %>%
  mutate(absoulte_error = abs(prediction - real),
         squared_error = (prediction - real)^2,
         relative_error = abs(prediction - real) / (real + 1)) %>%
  group_by(experiment, county, shift) %>%
  summarise(mean_absolute_error = mean(absoulte_error),
            mean_squared_error = mean(squared_error),
            mean_relative_error = mean(relative_error)) %>%
  ungroup()

mean_abs_error_by_shift <- mean_absolute_error_county_shift %>%
  # filter(time >= 234) %>%
  group_by(experiment, shift) %>%
  summarise(mean_absolute_error = mean(mean_absolute_error),
            mean_squared_error = mean(mean_squared_error),
            mean_relative_error = mean(mean_relative_error)) %>%
  ungroup()

view(mean_abs_error_by_shift %>%
  filter(shift < 7) %>%
  group_by(experiment) %>% 
  summarise(mean_absolute_error_f = round(mean(mean_absolute_error), digits = 2),
            sd = sd(mean_absolute_error)))

mean_abs_error_by_shift_plot <- mean_abs_error_by_shift %>% 
  mutate(shift = shift + 1,
         experiment = factor(experiment, levels = c("LAST_DAY", "AVG_WEEK", "FEED_FORWARD", "FEED_FORWARD + kappa", "MPNN + identity", "MPNN", "MPNN + binary", "MPNN + kappa", "MPNN_LSTM_skip + identity", "MPNN_LSTM_skip", "MPNN_LSTM_skip + binary", "MPNN_LSTM_skip + kappa", "MPNN_LSTM"))
         ) %>%
  filter(experiment %in% c("AVG_WEEK", "LAST_DAY", "FEED_FORWARD + kappa", "MPNN", "MPNN + binary", "MPNN_LSTM_skip", "MPNN_LSTM_skip + binary"))
    



ggplot(mean_abs_error_by_shift_plot, aes(x = shift, y = mean_absolute_error, group = experiment, color = experiment)) +
  geom_point(aes(shape = experiment), size = 4) +
  # scale_color_manual(values = c("#00C08B", "#FF64B0","#C77CFF","#00BA38" , "#00BFC4", "#B79F00", "#F8766D", "#00B4F0", "#F564E3", "#619CFF", "#7CAE00", "#DE8C00", "#FF65AC")) +
  scale_color_manual(values = c("#00C08B", "#FF64B0", "#00BA38", "#B79F00", "#F8766D", "#619CFF", "#7CAE00")) +
  # scale_shape_manual(values = c(19, 17, 2, 3, 5, 8, 13, 11, 9, 15, 7, 0, 21)) +
  scale_shape_manual(values = c(19, 17, 3, 8, 13, 15, 7)) +
  scale_x_continuous(labels = as.character(mean_abs_error_by_shift_plot$shift), breaks = mean_abs_error_by_shift_plot$shift) +
  xlab("Prediction for dt days ahead") +
  ylab("MAE in number of new cases\nper 100,000 inhabitants per county") +
  geom_line(size = 1) +
  # guides(color=guide_legend(nrow=4,byrow=F)) + 
  theme_bw() +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        axis.title = ggtext::element_markdown(size = 22, face="bold"),
        axis.text = element_text(size=20))



# ------------------------ TREND ANALYSIS --------------------------------

lk_heilbronn <- results_combined %>%
  filter(county == "Landkreis Heilbronn",
         start == as.Date("2020-10-13"),
         experiment == "MPNN_LSTM_skip + kappa") %>%
  mutate(shift = shift + 1)

summary(lm(prediction ~ shift, data = lk_heilbronn))


lm_fits <- results_combined %>%
  mutate(shift = shift + 1) %>%
  filter(start <= (as.Date("2020-12-31") - 14)) %>%
  pivot_longer(c("real", "prediction"), names_to = "type", values_to = "cases") %>%
  group_by(experiment, county, type, start) %>%
  do(fitCases = tidy(lm(cases ~ shift, data = .))) %>%
  unnest(fitCases)

classified_fits <- lm_fits %>%
  filter(term == "shift") %>%
  mutate(class = if_else(estimate < -1,
                         if_else(p.value < 0.05 | is.na(p.value),
                                 "Neg",
                                 "Neg-Inc."),
                         if_else(estimate <= 1,
                                 if_else(p.value < 0.05 | is.na(p.value),
                                         "N",
                                         "N-Inc."),
                                 if_else(p.value < 0.05 | is.na(p.value),
                                         "Pos",
                                         "Pos-Inc."))),
         class = factor(class, levels = c("Neg", "Neg-Inc.", "N", "N-Inc.", "Pos", "Pos-Inc."))) %>%
  select(experiment, county, type, start, estimate, p.value, class) 

write_csv(classified_fits, file = "trend_fits_100k_t7.csv")

classified_fits <- read_csv("../trend_fits_100k_t1.csv") %>%
  mutate(class = factor(class, levels = c("Neg", "Neg-Inc.", "N", "N-Inc.", "Pos", "Pos-Inc.")))

classified_fits_wide <- classified_fits %>% 
  filter(type == "prediction") %>%
  full_join(classified_fits %>% 
              filter(type == "real"), by = c("experiment", "county", "start"), suffix = c(".pred", ".obs"))

experiments <- unique(results_combined$experiment)

classified_fits_wide_corr <- classified_fits %>%
  filter(experiment != "MPNN_LSTM") %>%
  pivot_wider(c("experiment", "county", "start"), names_from = "type", values_from = "estimate")



cor_pear <- classified_fits_wide_corr %>%
  group_by(experiment) %>%
  do(corr_pearson = tidy(cor.test(~ real + prediction, method="pearson", data = .))) %>%
  unnest(corr_pearson)

cor_spear <- classified_fits_wide_corr %>%
  group_by(experiment) %>%
  do(corr_spearman = tidy(cor.test(~ real + prediction, method="spearman", data = .))) %>%
  unnest(corr_spearman)

cor_pear %>%
  mutate(estimate = round(estimate, digits = 2),
         p.value = round(p.value, digits = 3))
  
cor_spear %>%
  mutate(estimate = round(estimate, digits = 2),
         p.value = round(p.value, digits = 3))
  

test <- classified_fits_wide %>%
  filter(experiment == "MPNN")
cor.test(test$real, test$prediction)


get_metrics_and_plot_confusion_matrix <- function(filter_experiment, tag_input, only_pos_neg = FALSE, no_legend = FALSE){
  exper_fits <- classified_fits_wide %>%
    filter(experiment == filter_experiment)

  
  confusion_matrix <- as.data.frame(confusionMatrix(exper_fits$class.pred, exper_fits$class.obs)$table)
  metrics <- as.data.frame(confusionMatrix(exper_fits$class.pred, exper_fits$class.obs)$byClass) %>%
    rownames_to_column() %>%
    rename(class = rowname) %>%
    mutate(experiment = filter_experiment,
           class = substring(class, 8))
  
  if(only_pos_neg){
    confusion_matrix <- confusion_matrix %>%
      filter(Reference %in% c("Pos", "Neg"))
  }

  confusion_matrix <- confusion_matrix %>%
    group_by(Reference) %>%
    mutate(target_sum = sum(Freq),
           `P(Prediction|Target)` = Freq / target_sum,
           `P(Prediction|Target)` = if_else(is.nan(`P(Prediction|Target)`), 0, `P(Prediction|Target)`),
           target_perc_round = round(`P(Prediction|Target)` * 100, digits = 1))

 plot <-  ggplot(confusion_matrix, aes(x = Prediction, y = Reference, fill= `P(Prediction|Target)`)) +
    geom_tile() + 
    geom_text(aes(label=Freq)) + 
    geom_text(aes(label=paste(target_perc_round,"%", sep = "")), nudge_y = -0.3, size = 3.5, fontface = "italic") +
    scale_fill_gradient(limits = c(0, 1), low="white", high="#F8766D", labels = percent) +
    scale_x_discrete(position = "top") +
   scale_y_discrete(limits=rev) +
    ylab(label = "Observation") +
   # labs(tag = tag_input) + 
    theme_bw() +
   theme(
     # axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
         axis.title=element_text(size = 15, face="bold"),
         legend.title = element_text(face = "bold"),
         axis.text = element_text(size = 13),
         plot.tag = element_text(size = 16, face = "bold"))
 
 if(no_legend){
   plot <- plot +
     theme(legend.position = "none")
 }
 
 plot
 return(list(metrics=metrics, plot=plot))
}

last_day <- get_metrics_and_plot_confusion_matrix("LAST_DAY", tag_input = "A", only_pos_neg = F, no_legend = T)
avg_week <- get_metrics_and_plot_confusion_matrix("AVG_WEEK", tag_input = "B", only_pos_neg = FALSE, no_legend = F)

ff <- get_metrics_and_plot_confusion_matrix("FEED_FORWARD", tag_input = "C", only_pos_neg = FALSE, no_legend = T)
ff_k <- get_metrics_and_plot_confusion_matrix("FEED_FORWARD + kappa", tag_input = "D", only_pos_neg = FALSE, no_legend = F)

mpnn_i <- get_metrics_and_plot_confusion_matrix("MPNN + identity", tag_input = "E", only_pos_neg = FALSE, no_legend = T)
mpnn <- get_metrics_and_plot_confusion_matrix("MPNN", tag_input = "F", only_pos_neg = FALSE, no_legend = FALSE)

mpnn_b <- get_metrics_and_plot_confusion_matrix("MPNN + binary", tag_input = "G", only_pos_neg = F, no_legend = T)
mpnn_k <- get_metrics_and_plot_confusion_matrix("MPNN + kappa", tag_input = "H", only_pos_neg = FALSE, no_legend = T)

mpnn_lstm_i <- get_metrics_and_plot_confusion_matrix("MPNN_LSTM_skip + identity", tag_input = "I", only_pos_neg = FALSE, no_legend = T)
mpnn_lstm <- get_metrics_and_plot_confusion_matrix("MPNN_LSTM_skip", tag_input = "J", only_pos_neg = F, no_legend = F)

mpnn_lstm_b <- get_metrics_and_plot_confusion_matrix("MPNN_LSTM_skip + binary", tag_input = "K", only_pos_neg = F, no_legend = T)
mpnn_lstm_k <- get_metrics_and_plot_confusion_matrix("MPNN_LSTM_skip + kappa", tag_input = "L", only_pos_neg = FALSE, no_legend = F)

metrics_combined <- rbind(
      last_day$metrics %>% mutate(id = "1"),
      avg_week$metrics %>% mutate(id = "2"),
      ff$metrics %>% mutate(id = "3"),
      ff_k$metrics %>% mutate(id = "4"),
      mpnn_i$metrics %>% mutate(id = "5"),
      mpnn$metrics %>% mutate(id = "6"),
      mpnn_b$metrics %>% mutate(id = "7"),
      mpnn_k$metrics %>% mutate(id = "8"),
      mpnn_lstm_i$metrics %>% mutate(id = "9"),
      mpnn_lstm$metrics %>% mutate(id = "10"),
      mpnn_lstm_b$metrics %>% mutate(id = "11"),
      mpnn_lstm_k$metrics %>% mutate(id = "12")) %>%
  mutate(id = factor(id, ordered = T, levels = c("1", "2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
         experiment = factor(experiment, levels = c("LAST_DAY", "AVG_WEEK", "FEED_FORWARD", "FEED_FORWARD + kappa", "MPNN + identity", "MPNN", "MPNN + binary", "MPNN + kappa", "MPNN_LSTM_skip + identity", "MPNN_LSTM_skip", "MPNN_LSTM_skip + binary", "MPNN_LSTM_skip + kappa")))

<<results=tex>>
xtable(metrics_combined %>%
  # filter(experiment %in% c("LAST_DAY", "AVG_WEEK", "FEED_FORWARD", "FEED_FORWARD + kappa")) %>%
  # filter(experiment %in% c("MPNN + identity", "MPNN", "MPNN + binary", "MPNN + kappa")) %>%
  filter(experiment %in% c("MPNN_LSTM_skip + identity", "MPNN_LSTM_skip", "MPNN_LSTM_skip + binary", "MPNN_LSTM_skip + kappa")) %>%
  select(Experiment = experiment, Class = class, Sensitivity, Specificity, Precision, F1))

# POS SENSTIVITY
ggplot(metrics_combined %>% filter(class == "Pos"), 
       aes(x=id, y = Sensitivity, fill=experiment)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_color_manual(values = c("#F8766D", "#DE8C00", "#B79F00", "#7CAE00", "#00BA38", "#00C08B", "#00BFC4", "#00B4F0", "#619CFF", "#C77CFF", "#F564E3", "#FF64B0")) +
  xlab("Experiment") + 
  theme_bw() +
  theme(axis.title=element_text(size=34,face="bold"),
        axis.text = element_text(size=30),
        legend.title = element_blank(),
        legend.position = "bottom")

# POS PRECISION
ggplot(metrics_combined %>% filter(class == "Pos"), 
       aes(x=id, y = Precision, fill=experiment)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_color_manual(values = c("#F8766D", "#DE8C00", "#B79F00", "#7CAE00", "#00BA38", "#00C08B", "#00BFC4", "#00B4F0", "#619CFF", "#C77CFF", "#F564E3", "#FF64B0")) +
  xlab("Experiment") + 
  theme_bw() +
  theme(axis.title=element_text(size=34,face="bold"),
        axis.text = element_text(size=30),
        legend.title = element_blank(),
        legend.position = "bottom")

# NEG SENSITIVITY
ggplot(metrics_combined %>% filter(class == "Neg"), 
       aes(x=id, y = Sensitivity, fill=experiment)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_color_manual(values = c("#F8766D", "#DE8C00", "#B79F00", "#7CAE00", "#00BA38", "#00C08B", "#00BFC4", "#00B4F0", "#619CFF", "#C77CFF", "#F564E3", "#FF64B0")) +
  xlab("Experiment") + 
  theme_bw() +
  theme(axis.title=element_text(size=34,face="bold"),
        axis.text = element_text(size=30),
        legend.title = element_blank(),
        legend.position = "bottom")

# NEG PRECISION
ggplot(metrics_combined %>% filter(class == "Neg"), 
       aes(x=id, y = Precision, fill=experiment)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_color_manual(values = c("#F8766D", "#DE8C00", "#B79F00", "#7CAE00", "#00BA38", "#00C08B", "#00BFC4", "#00B4F0", "#619CFF", "#C77CFF", "#F564E3", "#FF64B0")) +
  xlab("Experiment") + 
  theme_bw() +
  theme(axis.title=element_text(size=34,face="bold"),
        axis.text = element_text(size=30),
        legend.title = element_blank(),
        legend.position = "bottom")


# POS + NEG SENSTIVITY
ggplot(metrics_combined %>% 
         filter(class %in% c("Pos", "Neg")) %>%
         mutate(Precision = if_else(is.na(Precision) | Precision == 0, 0, Precision)), 
       aes(x=class, y = Sensitivity, fill=experiment)) +
  geom_bar(stat="identity", position = "dodge", color="black") +
  # geom_hline(yintercept = 0, linetype="dashed", size=1) +
  scale_fill_manual(values = c("#00C08B", "#FF64B0","#C77CFF","#00BA38" , "#00BFC4", "#B79F00", "#F8766D", "#00B4F0", "#F564E3", "#619CFF", "#7CAE00", "#DE8C00")) +
  ylab("In-Class Sensitivity") + 
  xlab("Class") + 
  theme_bw() +
  guides(fill = guide_legend(nrow = 4, byrow = T)) +
  theme(axis.title=element_text(size=36,face="bold"),
        axis.text = element_text(size=34),
        legend.title = element_blank(),
        legend.text = element_text(size=26),
        legend.position = "top")


# POS + NEG Precision
ggplot(metrics_combined %>% 
         filter(class %in% c("Pos", "Neg")) %>%
         mutate(Precision = if_else(is.na(Precision) | Precision == 0, 0, Precision)), 
       aes(x=class, y = Precision, fill=experiment)) +
  geom_bar(stat="identity", position = "dodge", color="black") +
  # geom_hline(yintercept = 0, linetype="dashed", size=1) +
  scale_fill_manual(values = c("#00C08B", "#FF64B0","#C77CFF","#00BA38" , "#00BFC4", "#B79F00", "#F8766D", "#00B4F0", "#F564E3", "#619CFF", "#7CAE00", "#DE8C00")) +
  ylab("In-Class Precision") + 
  xlab("Class") + 
  theme_bw() +
  guides(fill = guide_legend(nrow = 4, byrow = T)) +
  theme(axis.title=element_text(size=36,face="bold"),
        axis.text = element_text(size=34),
        legend.title = element_blank(),
        legend.text = element_text(size=26),
        legend.position = "top")

ggplot(classified_fits %>% 
         filter(experiment != "MPNN_LSTM") %>% 
         pivot_wider(c("experiment", "county", "start"), names_from = "type", values_from = "estimate") %>%
         mutate(experiment = factor(experiment, levels = c("LAST_DAY", "AVG_WEEK", "FEED_FORWARD", "FEED_FORWARD + kappa", "MPNN + identity", "MPNN", "MPNN + binary", "MPNN + kappa", "MPNN_LSTM_skip + identity", "MPNN_LSTM_skip", "MPNN_LSTM_skip + binary", "MPNN_LSTM_skip + kappa"))) %>%
         # filter(experiment %in% c("LAST_DAY", "AVG_WEEK", "FEED_FORWARD", "FEED_FORWARD + kappa", "MPNN + identity", "MPNN")),
         filter(experiment %in% c("MPNN + binary", "MPNN + kappa", "MPNN_LSTM_skip + identity", "MPNN_LSTM_skip", "MPNN_LSTM_skip + binary", "MPNN_LSTM_skip + kappa")),
       aes(x=prediction, y = real, color = experiment)) +
  geom_point(alpha=0.4, size=3) +
  # scale_color_manual(values = c("#00C08B", "#FF64B0","#C77CFF","#00BA38" , "#00BFC4", "#B79F00")) +
  scale_color_manual(values = c("#F8766D", "#00B4F0", "#F564E3", "#619CFF", "#7CAE00", "#DE8C00")) +
  ylab("Slope (Observed)") + 
  xlab("Slope (Prediction)") + 
  theme_bw() +
  theme(axis.title=element_text(size=30,face="bold"),
        axis.text = element_text(size=28),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.text.y = element_text(size = 14)) +
  facet_grid(experiment ~ .)


ggplot(classified_fits %>% 
         filter(experiment != "MPNN_LSTM") %>% 
         pivot_wider(c("experiment", "county", "start"), names_from = "type", values_from = "estimate") %>%
         mutate(experiment = factor(experiment, levels = c("LAST_DAY", "AVG_WEEK", "FEED_FORWARD", "FEED_FORWARD + kappa", "MPNN + identity", "MPNN", "MPNN + binary", "MPNN + kappa", "MPNN_LSTM_skip + identity", "MPNN_LSTM_skip", "MPNN_LSTM_skip + binary", "MPNN_LSTM_skip + kappa"))) %>%
         filter(experiment == "MPNN"),
       aes(x=prediction, y = real, color = experiment)) +
  geom_point(alpha=0.4, size=3) +
  scale_color_manual(values = c("#00C08B", "#FF64B0","#C77CFF","#00BA38" , "#00BFC4", "#B79F00", "#F8766D", "#00B4F0", "#F564E3", "#619CFF", "#7CAE00", "#DE8C00")) +
  ylab("Slope (Observed)") + 
  xlab("Slope (Prediction)") + 
  theme_bw() +
  theme(axis.title=element_text(size=30,face="bold"),
        axis.text = element_text(size=28),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.text.y = element_text(size = 14))
