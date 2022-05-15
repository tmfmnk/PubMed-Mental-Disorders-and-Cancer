#Libraries

library(tidyverse)
library(RCurl)

#Import PubMed data
#All occurrences of keywords

anxiety_all_records <- read.csv(text = getURL("https://raw.githubusercontent.com/tmfmnk/PubMed-Mental-Disorders-and-Cancer/main/PubMed_data/anxiety_all_records.csv"),
                                skip = 1)

depression_all_records <- read.csv(text = getURL("https://raw.githubusercontent.com/tmfmnk/PubMed-Mental-Disorders-and-Cancer/main/PubMed_data/depression_all_records.csv"),
                                   skip = 1)

schizophrenia_all_records <- read.csv(text = getURL("https://raw.githubusercontent.com/tmfmnk/PubMed-Mental-Disorders-and-Cancer/main/PubMed_data/schizophrenia_all_records.csv"),
                                      skip = 1)

cancer_all_records <- read.csv(text = getURL("https://raw.githubusercontent.com/tmfmnk/PubMed-Mental-Disorders-and-Cancer/main/PubMed_data/cancer_all_records.csv"),
                               skip = 1)

#Keywords in title only

anxiety_title_only <- read.csv(text = getURL("https://raw.githubusercontent.com/tmfmnk/PubMed-Mental-Disorders-and-Cancer/main/PubMed_data/anxiety_title_only.csv"),
                               skip = 1)

depression_title_only <- read.csv(text = getURL("https://raw.githubusercontent.com/tmfmnk/PubMed-Mental-Disorders-and-Cancer/main/PubMed_data/depression_title_only.csv"),
                                  skip = 1)

schizophrenia_title_only <- read.csv(text = getURL("https://raw.githubusercontent.com/tmfmnk/PubMed-Mental-Disorders-and-Cancer/main/PubMed_data/schizophrenia_title_only.csv"),
                                     skip = 1)

cancer_title_only <- read.csv(text = getURL("https://raw.githubusercontent.com/tmfmnk/PubMed-Mental-Disorders-and-Cancer/main/PubMed_data/cancer_title_only.csv"),
                                                          skip = 1)

#Combine data
#All occurrences of keywords

data_all_records <- imap(mget(ls(pattern = "all_records$")),
                         ~ .x %>%
                           rename_with(.f = function(x) str_replace(.y, "_all_records", "_count"),
                                       .cols = Count) %>%
                           rename_with(tolower,
                                       .cols = Year)) %>%
  reduce(full_join, by = c("year")) %>%
  filter(year >= 1950 & year < 2022)

#Keywords in title only

data_title_only <- imap(mget(ls(pattern = "title_only$")),
                        ~ .x %>%
                          rename_with(.f = function(x) str_replace(.y, "_title_only", "_count"),
                                      .cols = Count) %>%
                          rename_with(tolower,
                                      .cols = Year)) %>%
  reduce(full_join, by = c("year")) %>%
  filter(year >= 1950 & year < 2022)

#Finding when the count of publications for mental disorders reached the count of 
#publications for cancer
#All occurrences of keywords

data_proximity_all_records <- data_all_records %>%
  mutate(depression_cancer_count = map_int(depression_count, ~ cancer_count[which.min(abs(.x - cancer_count))]),
         depression_cancer_year = map_int(depression_count, ~ year[which.min(abs(.x - cancer_count))]),
         anxiety_cancer_count = map_int(anxiety_count, ~ cancer_count[which.min(abs(.x - cancer_count))]),
         anxiety_cancer_year = map_int(anxiety_count, ~ year[which.min(abs(.x - cancer_count))]),
         schizophrenia_cancer_count = map_int(schizophrenia_count, ~ cancer_count[which.min(abs(.x - cancer_count))]),
         schizophrenia_cancer_year = map_int(schizophrenia_count, ~ year[which.min(abs(.x - cancer_count))])) %>%
  slice(1)

#Keywords in title only

data_proximity_title_only <- data_title_only %>%
  mutate(depression_cancer_count = map_int(depression_count, ~ cancer_count[which.min(abs(.x - cancer_count))]),
         depression_cancer_year = map_int(depression_count, ~ year[which.min(abs(.x - cancer_count))]),
         anxiety_cancer_count = map_int(anxiety_count, ~ cancer_count[which.min(abs(.x - cancer_count))]),
         anxiety_cancer_year = map_int(anxiety_count, ~ year[which.min(abs(.x - cancer_count))]),
         schizophrenia_cancer_count = map_int(schizophrenia_count, ~ cancer_count[which.min(abs(.x - cancer_count))]),
         schizophrenia_cancer_year = map_int(schizophrenia_count, ~ year[which.min(abs(.x - cancer_count))])) %>%
  slice(1)

#Plot 
#All occurrences of keywords

p_all_records <- data_all_records %>%
 ggplot(aes(x = year)) +
 geom_line(aes(y = depression_count, color = "Depression", linetype = "Depression"), size = 1.15) +
 geom_line(aes(y = anxiety_count, color = "Anxiety", linetype = "Anxiety"), size = 1.15) +
  geom_line(aes(y = schizophrenia_count, color = "Schizophrenia", linetype = "Schizophrenia"), size = 1.15) +
 geom_line(aes(y = cancer_count, color = "Cancer", linetype = "Cancer"), size = 1.15) +
 scale_x_continuous(limits = c(1950, 2021),
                    breaks = c(seq(1950, 2021, 3), 2021)) +
 scale_y_continuous(limits = c(0, 300000),
                    labels = scales::comma,
                    breaks = seq(0, 300000, 25000)) +
 scale_linetype_manual(name = "",
                       values = c("Depression" = "twodash", 
                                  "Anxiety" = "longdash",
                                  "Schizophrenia" = "dotdash",
                                  "Cancer" = "solid")) +
 scale_color_manual(name = "",
                    values = c("Depression" = "darkred", 
                               "Anxiety" = "steelblue", 
                               "Schizophrenia" = "darkgreen",
                               "Cancer" = "grey")) +
  geom_segment(aes(x = year, y = depression_count, xend = depression_cancer_year, yend = depression_cancer_count), colour = "black", data = data_proximity_all_records) +
  geom_segment(aes(x = year, y = anxiety_count, xend = anxiety_cancer_year, yend = anxiety_cancer_count), colour = "black", data = data_proximity_all_records) +
  annotate(geom = "text", x = data_proximity_all_records[["depression_cancer_year"]] - 1, y = data_proximity_all_records[["depression_cancer_count"]] + 3000, label = data_proximity_all_records[["depression_cancer_year"]], vjust = "left", fontface = "bold") +
  annotate(geom = "text", x = data_proximity_all_records[["anxiety_cancer_year"]] - 1, y = data_proximity_all_records[["anxiety_cancer_count"]] + 3000, label = data_proximity_all_records[["anxiety_cancer_year"]], vjust = "left", fontface = "bold") +
  annotate(geom = "curve", x = 2021, y = 75000, xend = 2021, yend = data_proximity_all_records[["schizophrenia_count"]], curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 2021 - 3, y = 80000, label = "Still below 1950", vjust = "left", fontface = "bold") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, face = "bold"),
       axis.text.y = element_text(size = 12, face = "bold"),
       axis.title.x = element_text(size = 13, face = "bold"),
       axis.title.y = element_text(size = 13, face = "bold"),
       plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
       legend.text = element_text(size = 13, face = "bold"),
       legend.key = element_blank(),
       axis.ticks = element_blank(),
       panel.border = element_blank(), 
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), 
       panel.background = element_blank(),
       axis.line = element_line(colour = "black")) +
 ggtitle("Publications per keywords from 1950 to 2021 (PubMed)") +
 xlab("Year") +
 ylab("Count")

ggsave(p_all_records, 
       filename = "pubs_per_years_all_records.png",
       width = 30,
       height = 15,
       units = "cm")

#Keywords in title only

p_title_only <- data_title_only %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = depression_count, color = "Depression", linetype = "Depression"), size = 1.15) +
  geom_line(aes(y = anxiety_count, color = "Anxiety", linetype = "Anxiety"), size = 1.15) +
  geom_line(aes(y = schizophrenia_count, color = "Schizophrenia", linetype = "Schizophrenia"), size = 1.15) +
  geom_line(aes(y = cancer_count, color = "Cancer", linetype = "Cancer"), size = 1.15) +
  scale_x_continuous(limits = c(1950, 2021),
                     breaks = c(seq(1950, 2021, 3), 2021)) +
  scale_y_continuous(limits = c(0, 100000),
                     labels = scales::comma,
                     breaks = seq(0, 100000, 10000)) +
  scale_linetype_manual(name = "",
                        values = c("Depression" = "twodash", 
                                   "Anxiety" = "longdash",
                                   "Schizophrenia" = "dotdash",
                                   "Cancer" = "solid")) +
  scale_color_manual(name = "",
                     values = c("Depression" = "darkred", 
                                "Anxiety" = "steelblue", 
                                "Schizophrenia" = "darkgreen",
                                "Cancer" = "grey")) +
  geom_segment(aes(x = year, y = depression_count, xend = depression_cancer_year, yend = depression_cancer_count), colour = "black", data = data_proximity_title_only) +
  geom_segment(aes(x = year, y = anxiety_count, xend = anxiety_cancer_year, yend = anxiety_cancer_count), colour = "black", data = data_proximity_title_only) +
  geom_segment(aes(x = year, y = schizophrenia_count, xend = schizophrenia_cancer_year, yend = schizophrenia_cancer_count), colour = "black", data = data_proximity_title_only) +
  annotate(geom = "text", x = data_proximity_title_only[["depression_cancer_year"]] - 1, y = data_proximity_title_only[["depression_cancer_count"]] + 1500, label = data_proximity_title_only[["depression_cancer_year"]], vjust = "left", fontface = "bold") +
  annotate(geom = "text", x = data_proximity_title_only[["anxiety_cancer_year"]] - 1, y = data_proximity_title_only[["anxiety_cancer_count"]] + 1500, label = data_proximity_title_only[["anxiety_cancer_year"]], vjust = "left", fontface = "bold") +
  annotate(geom = "text", x = data_proximity_title_only[["schizophrenia_cancer_year"]] - 1, y = data_proximity_title_only[["schizophrenia_cancer_count"]] + 1500, label = data_proximity_title_only[["schizophrenia_cancer_year"]], vjust = "left", fontface = "bold") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        legend.text = element_text(size = 13, face = "bold"),
        legend.key = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggtitle("Publications per keywords (title only) from 1950 to 2021 (PubMed)") +
  xlab("Year") +
  ylab("Count")

ggsave(p_title_only, 
       filename = "pubs_per_years_title_only.png",
       width = 30,
       height = 15,
       units = "cm")
