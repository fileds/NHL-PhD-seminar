rm(list = ls())
library(dplyr)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggplot2)


df <- read.table("dbs/fantasy_db.csv", header = TRUE)
df <- df %>%
  mutate(fantasyAvg = fantasyPoints / games) 

df %>%
  filter(fantasyPoints > 100) %>%
  ggplot(aes(x = fantasyPoints)) +
  geom_histogram(aes(fill = position))

df %>% 
  ggplot(aes(x = fantasyPoints)) + 
  geom_density()

picked_c <- df %>%
  filter(season == 2021) %>%
  filter(position == "C") %>%
  arrange(desc(fantasyPoints)) %>%
  head(30) %>%
  mutate(gain = fantasyPoints - min(fantasyPoints)) %>%
  mutate(avgGain = fantasyAvg - min(fantasyAvg))

picked_w <- df %>%
  filter(season == 2021) %>%
  filter(position == "W") %>%
  arrange(desc(fantasyPoints)) %>%
  head(60) %>%
  mutate(gain = fantasyPoints - min(fantasyPoints)) %>%
  mutate(avgGain = fantasyAvg - min(fantasyAvg))

picked_d <- df %>%
  filter(season == 2021) %>%
  filter(position == "D") %>%
  arrange(desc(fantasyPoints)) %>%
  head(50) %>%
  mutate(gain = fantasyPoints - min(fantasyPoints)) %>%
  mutate(avgGain = fantasyAvg - min(fantasyAvg))

picked_g <- df %>%
  filter(season == 2021) %>%
  filter(position == "G") %>%
  arrange(desc(fantasyPoints)) %>%
  head(20) %>%
  mutate(gain = fantasyPoints - min(fantasyPoints)) %>%
  mutate(avgGain = fantasyAvg - min(fantasyAvg))

picked <- rbind(picked_g, picked_d, picked_w, picked_c)
picked %>%
  ggplot(aes(x = fantasyPoints)) + 
  geom_density(aes(fill = position), alpha = 0.4)
picked %>%
  filter(position != "G") %>%
  ggplot(aes(x = fantasyAvg)) + 
  geom_density(aes(fill = position), alpha = 0.4)
picked %>%
  ggplot(aes(x = gain)) + 
  geom_density(aes(fill = position), alpha = 0.4)

team_factor <- df %>%
  filter(season > 2020) %>%
  group_by(season, team) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  as.data.frame() %>%
  select(season, team, fantasyPoints) %>%
  mutate(teamFactor = fantasyPoints / max(fantasyPoints)) %>%
  select(team, teamFactor) # Add season

# Join by season as well
player_df <- merge(df, team_factor, by = "team")
  
player_df <- player_df %>%
  filter(season > 2010) %>%
  mutate(fantasyAvg = fantasyPoints / games) %>%
  group_by(id, season) %>%
  mutate(fantasyAvg = fantasyAvg / teamFactor) %>%
  select(id, season, fantasyAvg) %>%
  as.data.frame()

player_time_series <- dcast(player_df, id ~ season, value.var = 'fantasyAvg')

ar(player_time_series[,2:11], na.action = na.exclude)
  

# Autoregressive model
# 1. standardize model (average instead) 
# (2. standardize over team)
