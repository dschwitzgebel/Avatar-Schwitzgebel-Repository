library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pwr)
library(lme4)
library(lmerTest)
library(car)

############################ t-test ############################

all_data <- read_csv("Experiment_1_Data.csv")

all_data <- all_data %>%
  filter(AttentionCheck != 0)

all_data <- all_data %>%
  mutate(Q2 = if_else((Q2 == "Aang"), 1, 0)) %>%
  mutate(Q3 = if_else((Q3 == "Katara and Sokka"), 1, 0)) %>%
  mutate(Q4 = if_else((Q4 == "They are reincarnated from the previous Avatar, cycling between Nations over time."), 1, 0)) %>%
  mutate(Q5 = if_else((Q5 == "The south and north poles"), 1, 0)) %>%
  mutate(Q6 = if_else((Q6 == "Takes away his bending"), 1, 0)) %>%
  mutate(Q7 = if_else((Q7 == "Enhance the power of fire bending"), 1, 0)) %>%
  mutate(accuracy = rowSums(.[20:25])) %>%
  rowwise()%>%
  mutate(experience = if_else(((accuracy > 3) & (as.numeric(Q2_1) > 3)), 1, 0)) %>%
  mutate(diff_score = (mean(c(Q34_1, Q75_1, Q76_1)) - mean(c(Q77_1, Q78_1, Q79_1)))) %>%
  select(PID, Q34_1, Q75_1, Q76_1, Q77_1, Q78_1, Q79_1, experience, diff_score)

t.test(diff_score ~ experience, data=all_data, var.equal = TRUE)

############################ table ############################

all_data <- read_csv("Experiment_1_Data.csv")

all_data <- all_data %>%
  filter(AttentionCheck != 0)

all_data <- all_data %>%
  mutate(Q2 = if_else((Q2 == "Aang"), 1, 0)) %>%
  mutate(Q3 = if_else((Q3 == "Katara and Sokka"), 1, 0)) %>%
  mutate(Q4 = if_else((Q4 == "They are reincarnated from the previous Avatar, cycling between Nations over time."), 1, 0)) %>%
  mutate(Q5 = if_else((Q5 == "The south and north poles"), 1, 0)) %>%
  mutate(Q6 = if_else((Q6 == "Takes away his bending"), 1, 0)) %>%
  mutate(Q7 = if_else((Q7 == "Enhance the power of fire bending"), 1, 0)) %>%
  mutate(accuracy = rowSums(.[20:25])) %>%
  rowwise()%>%
  mutate(experience = if_else(((accuracy > 3) & (as.numeric(Q2_1) > 3)), 1, 0)) %>%
  mutate(diff_score = (mean(c(Q34_1, Q75_1, Q76_1)) - mean(c(Q77_1, Q78_1, Q79_1)))) %>%
  select(PID, Q34_1, Q75_1, Q76_1, Q77_1, Q78_1, Q79_1, experience)

all_data <- all_data %>% 
  gather(clip, response, -c(PID, experience)) %>%
  mutate(clip_character = if_else((clip == "Q34_1" | clip == "Q75_1" | clip == "Q76_1"), "Iroh", "Katara"))

data_table <- all_data %>%
  group_by(experience, clip_character) %>%
  summarize(response = mean(response))


############################ generalized linear model ############################

all_data <- read_csv("Experiment_1_Data.csv")

all_data <- all_data %>%
  filter(AttentionCheck != 0)

all_data <- all_data %>%
  mutate(Q2 = if_else((Q2 == "Aang"), 1, 0)) %>%
  mutate(Q3 = if_else((Q3 == "Katara and Sokka"), 1, 0)) %>%
  mutate(Q4 = if_else((Q4 == "They are reincarnated from the previous Avatar, cycling between Nations over time."), 1, 0)) %>%
  mutate(Q5 = if_else((Q5 == "The south and north poles"), 1, 0)) %>%
  mutate(Q6 = if_else((Q6 == "Takes away his bending"), 1, 0)) %>%
  mutate(Q7 = if_else((Q7 == "Enhance the power of fire bending"), 1, 0)) %>%
  mutate(accuracy = rowSums(.[20:25])) %>%
  rowwise()%>%
  mutate(experience = if_else(((accuracy > 3) & (as.numeric(Q2_1) > 3)), 1, 0)) %>%
  mutate(diff_score = (mean(c(Q34_1, Q75_1, Q76_1)) - mean(c(Q77_1, Q78_1, Q79_1)))) %>%
  select(PID, Q34_1, Q75_1, Q76_1, Q77_1, Q78_1, Q79_1, experience)

all_data <- all_data %>% 
  gather(clip, response, -c(PID, experience)) %>%
  mutate(clip_character = if_else((clip == "Q34_1" | clip == "Q75_1" | clip == "Q76_1"), "Iroh", "Katara"))

glm_analysis <- glmer(response ~ clip_character * experience + (1|PID) + (1|clip), data=all_data)
summary(glm_analysis)

Anova(glm_analysis, type=3)
