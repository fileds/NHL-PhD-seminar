library(dplyr)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(tidyverse)

df <- read.table("dbs/fantasy_db.csv", header = T)

# Filtering player with name containing "Ovechkin"
df %>% filter(grepl("Ovechkin", fullName))

# Selecting non-golie columns. Remove exclamation mark to select only goalie 
# columns
df %>% 
  filter(grepl("Ovechkin", fullName)) %>% 
  select(!c("shutouts", "ties", "wins", "losses", "saves", "shotsAgainst",
  "goalsAgainst", "ot", "powerPlaySaves", "shortHandedSaves", "evenSaves", 
  "shortHandedShots", "evenShots", "powerPlayShots", "gamesStarted", 
  "powerPlaySavePercentage", "shortHandedSavePercentage", 
  "evenStrengthSavePercentage"))

df %>%
  mutate(goals = as.numeric(goals),
    timeOnIce = as.numeric(timeOnIce))
  mutate(goalPerMinute = case_when(
    !is.na(goals) & !is.na(timeOnIce) ~ goals / timeOnIce,
    T ~ NA))
