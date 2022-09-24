rm(list = ls())
library(dplyr)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggplot2)


df <- read.table("dbs/fantasy_db.csv", header = TRUE)

# Getting team effect. Team effect currently only considers last season. In 
# order to use historic data retired players need to be included.
# TODO: Include retired players.
team_factor <- df %>%
  filter(season > 2020) %>%
  group_by(season, team) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  as.data.frame() %>%
  select(season, team, fantasyPoints) %>%
  mutate(teamFactor = fantasyPoints / max(fantasyPoints)) %>%
  select(team, teamFactor) # Add season

# Creating df for autoregression. 
ar_df <- merge(df, team_factor, by = "team")
  
# Subsetting to get last 12 seasons. Create average score. Accounting for team 
# factor. 
ar_df <- ar_df %>%
  filter(season > 2010) %>%
  mutate(fantasyAvg = fantasyPoints / games) %>%
  group_by(id, season) %>%
  mutate(fantasyAvg = fantasyAvg / teamFactor) %>%
  select(id, season, fantasyAvg) %>%
  as.data.frame()

# Creating time series df
time_series <- dcast(ar_df, id ~ season, value.var = 'fantasyAvg')

# Performing autoregression
# TODO: Make it work :)
ar(time_series, na.action = na.exclude)
  

# Autoregressive model
# 1. standardize model (average instead) 
# (2. standardize over team)
