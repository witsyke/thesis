library(tidyverse)

setwd("<<PATH TO MOVEMENT DATA>>")
load("int_movement_prob_matrix.Rdata") # Q

top_travel_countries <- as.data.frame(int_movement_prob_matrix) %>%
    rownames_to_column("county") %>%
    pivot_longer(-county, names_to = "country", values_to = "probability") %>%
    select(-county) %>%
    group_by(country) %>%
    summarise(sum = sum(probability)) %>%
    slice_max(sum, n = 10)

top_vacation_countries <- c(
    "Spain", "Italy", "Turkey", "Austria", "Greece",
    "France", "Croatia", "Poland", "Netherlands", "Egypt")

load("int_active_cases.Rdata")

top_travel_country_cases <- int_active_cases %>%
    pivot_longer(-date, names_to = "country", values_to = "cases") %>%
    filter(country %in% top_travel_countries$country,
           date >= as.Date("2021-07-01"))


top_vacation_country_cases <- int_active_cases %>%
    pivot_longer(-date, names_to = "country", values_to = "cases") %>%
    filter(country %in% top_vacation_countries,
           date >= as.Date("2021-07-01"))

load("../case/est_active_cases_prop.RData")

ger <- est_active_cases_prop %>%
        filter(date >= as.Date("2021-07-01")) %>%
        mutate(country = "Germany (Simple Est.)") %>%
        select(date, country, cases = total_active_cases_prop)

load("../case/total_active_cases_prop.RData")

ger2 <- total_active_cases_prop %>%
        filter(Meldedatum >= as.Date("2021-07-01")) %>%
        mutate(country = "Germany (RKI)") %>%
        select(date = Meldedatum, country, cases = active_cases_prop)

top_travel_country_cases_ger <- top_travel_country_cases %>%
    union(ger) %>%
    union(ger2)

top_vacation_country_cases_ger <- top_vacation_country_cases %>%
    union(ger) %>%
    union(ger2)

library(directlabels)

ggplot(data = top_travel_country_cases_ger,
        aes(x = date, y = cases, group = country, color = country, linetype = country)) +
    geom_line() +
    ggtitle("Prevalence (%) for the top 10 travel destinations (extracted from NC) for Germany") +
    ylab("Prevalence (%)") +
    ylim(0, 0.026) +
    xlab("Month") +
    scale_x_date(expand = expansion(add = c(0, 35))) +
    geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
    scale_color_manual(breaks = c(
        "Belgium", "Italy", "Czech Republic",
        "Austria", "Denmark", "France",
        "Hungary", "Poland", "Netherlands",
        "Switzerland", "Germany (Simple Est.)", "Germany (RKI)"),
                        values = c(
                            "#003f5c", "#2f4b7c", "#665191",
                            "#a05195", "#d45087", "#f95d6a",
                            "#ff7c43", "#ffa600", "#bb4c02",
                            "#932003", "black", "black")) +
    scale_linetype_manual(breaks = c(
        "Belgium", "Italy", "Czech Republic",
        "Austria", "Denmark", "France",
        "Hungary", "Poland", "Netherlands",
        "Switzerland", "Germany (Simple Est.)", "Germany (RKI)"),
                            values = c(
                                "solid", "solid", "solid",
                                "solid", "solid", "solid",
                                "solid", "solid", "solid",
                                "solid", "longdash", "dotted")) +
    theme_bw() +
    theme(axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

ggplot(data = top_vacation_country_cases_ger, 
        aes(x = date, y = cases, group = country, color = country, linetype = country)) +
    geom_line() +
    ggtitle("Prevalence (%) for the top 10 vacation destinations (2019) for Germany") +
    ylab("Prevalence (%)") +
    ylim(0, 0.026) +
    xlab("Month") +
    scale_x_date(expand = expansion(add = c(0, 35))) +
    geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
    scale_color_manual(breaks = c(top_vacation_countries, "Germany (Simple Est.)", "Germany (RKI)"),
                        values = c(
                            "#003f5c", "#2f4b7c", "#665191",
                            "#a05195", "#d45087", "#f95d6a",
                            "#ff7c43", "#ffa600", "#bb4c02",
                            "#932003", "black", "black")) +
    scale_linetype_manual(breaks = c(top_vacation_countries, "Germany (Simple Est.)", "Germany (RKI)"),
                            values = c(
                                "solid", "solid", "solid",
                                "solid", "solid", "solid",
                                "solid", "solid", "solid",
                                "solid", "longdash", "dotted")) +
    theme_bw() +
    theme(axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))
