library(dplyr)
library(flexdashboard)
library(ggplot2)
library(ggsci)
library(plotly)
library(readr)
library(shiny)
library(stringr)
library(tibble)

# load the full dataset (setwd() to repo/lab04)
interaction_filtered <- read_csv("interaction_filtered.csv")

# keep only category_level = 1
interaction_filtered <- interaction_filtered %>%
  filter(category_level == 1)

# deduplicate PIDs
interaction_filtered <- interaction_filtered %>%
  group_by(pid) %>%
  slice(1) %>%
  ungroup()

write_csv(interaction_filtered, "interaction_filtered_small.csv")

# PIDs per user
interaction_filtered %>%
    filter(category_id == 2) %>%
    group_by(user_id, gender) %>%
    summarise(
        n_pids = n()
    ) %>% arrange(-n_pids)

interaction_filtered %>%
    filter(category_id == 4) %>%
    group_by(user_id, gender) %>%
    ggplot(aes(fill=gender, x=gender)) +
    geom_bar()
