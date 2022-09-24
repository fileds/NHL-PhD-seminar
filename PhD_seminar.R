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

# Mutating to get goals per minute
# Mutate helps you create new columns. It is powerful to use combined with 
# case_when().
df %>%
  mutate(goalPerMinute = as.numeric(as.numeric(goals) 
    / as.numeric(timeOnIce))) %>%
  filter(grepl("Ovechkin", fullName))
  
# Mutate with case_when()
df %>%
  mutate(manyGoals = case_when(
    goals > 30 ~ "strong",
    TRUE ~ "weak")
  ) %>%
  filter(grepl("Ovechkin", fullName))

# Creating column to compare goals scored between seasons
# group_by() is useful when we want to compare subsets of our sample. Here we 
# group on id to only compare seasons for one player. Lag is used to compare 
# with the previous row.
df <- df %>%
  group_by(id) %>%
  mutate(goalDiff = goals - lag(goals, 1)) %>%
  as.data.frame() %>%
  filter(grepl("Ovechkin", fullName))
  