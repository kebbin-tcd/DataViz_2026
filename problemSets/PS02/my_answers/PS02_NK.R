library(tidyverse)
lapply(c("tidyverse", "ggplot2"),  pkgTest)
library(dplyr)
library(readxl)
#Data Manipulation
#Part 1 loading datasets
ncss_data <- read.csv("/Users/nourkebbi/Documents/GitHub/DataViz_2026/problemSets/PS02/my_answers/NCSS_v1.csv", header = TRUE, sep = ",")
#head(ncss_data)
#filtering out columns
ncss_filtered <- nscc_data %>% select('CASEID','YEAR','GDREGION','NUMOFFMBR','TRAD6','TRAD12','INCOME')
#head(ncss_filtered)
#Part 2 filtering religions
#unique(ncss_filtered$TRAD6)
ncss_filtered <- ncss_filtered %>% filter(TRAD6 %in% c("Chrétiennes"_NK,"Juives","Musulmanes"))
#Part 3  pivoting
ncss_pivot <- ncss_filtered %>%
  filter(YEAR == max(YEAR, na.rm = TRUE)) %>%
  group_by(TRAD6, YEAR) %>%
  summarize(
    congressions = sum(NUMOFFMBR, na.rm = TRUE),
    mean_income = mean(INCOME, na.rm = TRUE),
    median_income = median(INCOME, na.rm = TRUE))
ncss_pivot
#Part 4 categorical value
ncss_pivot <- ncss_pivot %>%
  mutate(AVG_INCOME = if_else(mean_income >= congressions, 1, 0))
ncss_pivot

#Data Visualization
#Part 1 bar plot
ggplot(ncss_filtered, aes (x = factor(YEAR) , y = NUMOFFMBR , fill = TRAD12)) +
  geom_bar(stat = "identity" ) +
  labs (x = " \nYear " , y = " Congressions \n" , fill = "TRAD12 ")
#Part 2 histogram
ggplot(ncss_filtered, aes (x = TRAD6 , y = NUMOFFMBR, fill = TRAD12)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(YEAR)) +
  labs (x = " \nTRAD6" , y = " Congressions\n" , fill = "TRAD12")
#Part 3 distribution
ggplot(ncss_filtered, aes(x = INCOME, y = GDREGION, fill = INCOME)) +
  geom_jitter(alpha = 0.5, width = 0.2) + 
  labs(x = "Income", y = "Region", fill = "Income")
#Part 4 boxplot
ggplot(ncss_filtered, aes(x = factor(YEAR) ,y = NUMOFFMBR , fill = GDREGION)) +
  geom_boxplot(width = 0.5) + #geom_point(position = position_jitter(height = 1)) +
  labs (x = " \nYEAR " , y = " Congressions\n" )
