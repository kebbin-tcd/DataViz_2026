#Data Manipulation Part 1
library(tidyverse)
lapply(c("tidyverse", "ggplot2"),  pkgTest)
library(readxl)

#loading datasets
mep_info <- read_excel("/Users/nourkebbi/Documents/GitHub/DataViz_2026/problemSets/PS01/template/my_answers/mep_info_26Jul11.xls", sheet = "EP1")
rcv_info <- read.table("/Users/nourkebbi/Documents/GitHub/DataViz_2026/problemSets/PS01/template/my_answers/rcv_ep1.txt", header = TRUE, sep = ",")

#head(mep_info)
#head(rcv_info)
#print(colnames(mep_info))
#Data Manipulation Part 3A
rcv_tidy <- rcv_info %>%
  pivot_longer(
    cols = !c(MEPID, MEPNAME, MS, NP, EPG),
    names_to = "vote_id",
    values_to = "decision"
  )
mep_info <- mep_info %>% #renaming the column so I can join
  rename(MEPID = `MEP id`) 
mep_info <- mep_info %>% #casting it into an INT so I can join
  mutate(MEPID = as.integer(MEPID)) 
#head(rcv_tidy)
#Data Manipulation Part 3B
summary_table <- rcv_tidy %>%
  group_by(decision) %>%
  summarize(count= n())

#head(summary_table)
#Data Manipulation Part 4
joint_table <- left_join(rcv_tidy, mep_info, by = "MEPID")
colSums(is.na(joint_table))
joint_table <- joint_table %>%
  drop_na()
#head(joint_table)
#print(joint_table)

#Data Manipulation Part 5
avg_table <- joint_table %>%
  group_by(EPG) %>%
  summarize(
    mean_yes_votes = mean(decision == 1)/mean(decision %in% c(1,2,3)),
    mean_abstention = mean(decision == 3),
    mean_nomd1 = mean(`NOM-D1`, na.rm = TRUE),
    mean_nomd2 = mean(`NOM-D2`, na.rm = TRUE))

#head(avg_table)
#Data Visualization Part 1
ggplot(mep_info, aes(x = `NOM-D1`, fill = `EP Group`)) +
  geom_bar() +
  labs(title = "Distribution of the first NOMINATE dimension by EP group")

#Data Visualization Part 2
ggplot(mep_info, aes(x = `NOM-D1`, y = `NOM-D2`, color = `EP Group`)) +
  geom_point() +
  labs(title = "Scatterplot of nomdim1 and nomdim2")

#Data Visualization Part 3
dv_part3 <- joint_table %>%
  group_by(MEPID, EPG) %>%
  summarize(voting_yes = mean(decision == 1, na.rm = TRUE))
ggplot(dv_part3, aes(x = EPG, y = voting_yes)) +
  geom_boxplot() +
  labs(title = "The proportion voting Yes by EP group")

#Data Visualization Part 4
dv_part4 <- joint_table %>%
  group_by(`National Party`) %>%
  summarize(voting_yes = mean(decision == 1, na.rm = TRUE))
ggplot(dv_part4, aes(x = `National Party`, y = voting_yes)) +
  geom_bar(stat = "identity") +
  labs(title = "The proportion voting Yes by national party")

