library(tidyverse)
lapply(c("tidyverse", "ggplot2"),  pkgTest)
library(dplyr)
library(readxl)
#loading dataset
url <- "https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/CES2015.csv"
ces_data <- read.csv(url)
head(ces_data)
#data manipulation
#q1
ces2015 <- ces_data |> filter(discard == "Good quality")
#q2
ces2015 <- ces2015 |>
  mutate(p_voted = case_when(
    p_voted == "Don't know" ~ NA_character_,
    p_voted == "Refused"    ~ NA_character_,
    TRUE                   ~ p_voted
  ))
ces2015 <- ces2015 |> filter(!is.na(p_voted))
#structure(ces2015)
#unique(ces2015$p_voted)

#q3
#ces2015 <- ces2015 |> mutate(current_age = as.numeric(year) - as.numeric(age))
ces2015 <- ces2015 |>
  mutate(
    current_age = as.numeric(year) - as.numeric(age),
    age_group = cut(current_age, 
                    breaks = c(0, 30, 45, 65, Inf), labels = c("<30", "30-44", "45-64", "65+"),
                    right = FALSE))
#data visualization
#q1
turnout_by_age <- ces2015 |>
  group_by(age_group) |>
  summarise(turnout_rate = mean(p_voted == "Yes", na.rm = TRUE)) |>
  filter(!is.na(age_group)) 
#head(turnout_by_age)
#plot for q1
ggplot(turnout_by_age, aes(x = age_group, y = turnout_rate)) +
  geom_col(fill = "grey") +
  labs(title = "Turnout rate by age group",
       x = "Age Group",
       y = "Turnout Rate") +
  theme_minimal()
#q2
#names(ces2015)
#unique(ces2015$vote_for)
ideo_data <- ces2015 |>
  filter(!is.na(p_selfplace), 
         voted_for %in% c("Liberal", "Conservative", "NDP", "Bloc Quebecois", "Green")) |>
  mutate(left_right = as.numeric(p_selfplace),
         party_vote = voted_for)
#plot
ggplot(ideo_data, aes(x = left_right, fill = party_vote)) +
  geom_density(alpha = 0.5) +
  labs(title = "Ideology Density by Party (0-10 scale)",
       x = "Non-missing left-right aelf-placement (0-10)",
       y = "Density",
       fill = "Party") +
  scale_x_continuous(breaks = seq(0, 10, 1)) + 
  theme_minimal()
#q3
ggplot(ces2015, aes(x = income_full, fill = p_voted)) +
  geom_bar(position = "stack") + 
  facet_wrap(~ province) +
  labs(title = "Counts of turnout by income and province",
       x = "Income",
       y = "Count",
       fill = "Voted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#q4
library(ggrepel)
custom_theme <- function() {
  theme_minimal() + 
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 12, hjust = 0),
      plot.caption = element_text(size = 9, color = "grey", hjust = 0),
      axis.title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size = 8),
      legend.position = "right",
      panel.grid.major.x = element_blank(),
      panel.border = element_blank()
    )
}
#testing:
ggplot(turnout_by_age, aes(x = age_group, y = turnout_rate)) +
  geom_col(fill = "grey") +
  labs(title = "Turnout rate by age group",
       x = "Age Group",
       y = "Turnout Rate") +
  custom_theme()
ggplot(ces2015, aes(x = income_full, fill = p_voted)) +
  geom_bar(position = "stack") + 
  facet_wrap(~ province) +
  labs(title = "Counts of turnout by income and province",
       x = "Income",
       y = "Count",
       fill = "Voted") +
  custom_theme() 





