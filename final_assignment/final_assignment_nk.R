library(tidyverse)
library(scales)  # For nicer scales
install.packages("readxl")
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
#loading dataset
url <- "https://www.migrationpolicy.org/sites/default/files/datahub/MPI-Data-Hub_NaturalizationbyCOB_2023.xlsx"
destination_file <- "Migration_Data.xlsx"
download.file(url, destination_file, mode = "wb")
#the file has a header so I have to start from row 8
df <- read_excel(destination_file, skip = 7)
head(df)

#creating a theme
my_theme <- theme_minimal(base_family = "sans") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = rel(1.1)),
    strip.text = element_text(face = "bold", size = rel(1.3), color = "midnightblue"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95"),
    plot.background = element_rect(color = "grey20", fill = NA, size = 1),
    plot.margin = margin(10, 10, 10, 10) # Adds a little padding inside the frame
  )

#creating figures

#figure 1 - totals per year
df_figure_1 <- df[1, ]
#head(df_figure_1)
df_long <- df_figure_1 %>%
  pivot_longer(
    cols = -1,
    names_to = "Year",
    values_to = "Total"
  ) %>%
  mutate(Year = as.integer(Year), Total = as.numeric(Total))
ggplot(df_long, aes(x = Year, y = Total)) +
  geom_col(fill = "midnightblue", alpha = 0.8) +
  labs(
    title = "Total acquisition of US Citizenships by Year",
    x = "Year of Citizenship Acquisition",
    y = "Total persons acquiring US Citizenship",
    caption = "Figure 1"
  ) +
  my_theme

#figure 2 - totals per year by region
df_figure_2 <- df %>%
  slice(2, 56, 107, 157, 202, 215)
#head(df_figure_2)
df_long_regions <- df_figure_2 %>%
  pivot_longer(
    cols = -1,      
    names_to = "Year",
    values_to = "Total"
  ) %>%
  mutate(
    Year = as.integer(Year),
    Total = as.numeric(Total)
  )
colnames(df_long_regions)[1] <- "Region"
ggplot(df_long_regions, aes(x = Year, y = Total, group = Region)) +
  geom_line(color = "midnightblue", size = 1) +
  geom_point(color = "midnightblue", size = 1.5) +
  facet_wrap(~Region, scales = "free_y") + 
  labs(
    title = "Total acquisition of US Citizenships by Year and by Region",
    x = "Year",
    y = "Total persons acquiring US Citizenship",
    caption = "Figure 2"
  ) + my_theme


#figure 3: zooming into Americas with highest acquisition in 2008
df_americas <- df %>%
  slice(56:106) %>%
  select(1, `2008`)
df_americas <- df_americas %>%
  mutate(`2008` = as.integer(gsub("[^0-9]", "", `2008`)))
#glimpse(df_americas)

world_map <- map_data("world")
df_map <- df_americas %>%
  rename(region = 1) %>%
  mutate(region = recode(region, "United States" = "USA"))

map_data_complete <- inner_join(world_map, df_map, by = "region")
region_labels <- map_data_complete %>%
  group_by(region) %>%
  summarize(long = mean(long), lat = mean(lat), `2008` = first(`2008`))

ggplot(map_data_complete, aes(x = long, y = lat, group = group, fill = `2008`)) +
  geom_polygon(color = "white") +
  geom_text(data = region_labels, aes(label = region, group = NULL), 
            size = 3, check_overlap = TRUE) +
  scale_fill_gradient(low = "#e0f3f8", high = "#084594", na.value = "grey90") +
  labs(
    title = "Total acquisition of US Citizenships in 2008 in Americas",
    x = "Longitude", y = "Latitude", caption = "Figure 3", fill = "Total Persons"
  ) + my_theme +
 theme(legend.key.width = unit(1.5, "cm"))
