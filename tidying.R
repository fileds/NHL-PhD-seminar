library(dplyr)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(tidyverse)

# Loading seasons and adding fullName and position.
load_season <- function(path) {
  idf <- read.csv(paste0(path, "/info.csv"))
  sdf <- read.csv(paste0(path, "/seasons.csv"))
  sdf <- sdf %>% 
    mutate(fullName = idf$fullName,
      id = idf$id,
      age = idf$age,
      position = case_when(
        grepl("Goalie", idf$primaryPosition, fixed = TRUE) ~ "G",
        grepl("Defenseman", idf$primaryPosition, fixed = TRUE) ~ "D",
        grepl("Right Wing", idf$primaryPosition, fixed = TRUE) ~ "W",
        grepl("Left Wing", idf$primaryPosition, fixed = TRUE) ~ "W",
        grepl("Center", idf$primaryPosition, fixed = TRUE) ~ "C"
      )
    ) 
  
  return(sdf)
}

# Loading and merging into one large DF
ids <- list.dirs(path = "/home/fileds/dev/NHL/data_fetcher/data/player_data", 
  full.names = TRUE, recursive = FALSE)
df <- bind_rows(lapply(ids[1:length(ids)], load_season))

# Selects relevant columns, relocating and fixing string data to numeric.
df <- df %>% 
  select(!c(X, sequenceNumber)) %>%
  relocate(position) %>%
  relocate(fullName) %>%
  relocate(id)  %>%
  mutate(season = as.numeric(substr(season, 1, 4))) 

fantasy_df <- df %>%
  filter(league == "National Hockey League") %>%
  map_df(rev) %>%
  group_by(id, season) %>%
  summarise(across(), across(where(is.numeric), sum, na.rm = TRUE)) %>%
  distinct(season, .keep_all = TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(fantasyPoints = case_when(
      position == "G" ~ (2 * wins - 0.8 * goalsAgainst + 0.15 * saves + 
          3 * shutouts),
      TRUE ~ (2 * assists + 3 * goals + plusMinus * 0.3 + 1 * shortHandedPoints 
        + 0.2 * shots + 0.2 * hits + 0.3 * blocked))
    ) %>%
  separate(timeOnIce, c("timeOnIce", NA), ":", convert = TRUE) %>%
  separate(evenTimeOnIce, c("evenTimeOnIce", NA), ":", convert = TRUE) %>%
  separate(powerPlayTimeOnIce, c("powerPlayTimeOnIce", NA), ":", 
    convert = TRUE) %>%
  separate(shortHandedTimeOnIce, c("shortHandedTimeOnIce", NA), ":", 
    convert = TRUE)

  
  

fantasy_df_clean <- fantasy_df %>% 
  select(c(fullName, position, season, team, fantasyPoints, games, goals, 
    assists, points, plusMinus, shortHandedPoints, shots, hits, blocked, wins, 
    goalsAgainst, saves, shutouts))

fantasy_df_last_season <- fantasy_df %>% 
  filter(season == "20212022") %>% 
  arrange(desc(fantasyPoints))

fantasy_df_clean_last_season <- fantasy_df_clean %>% 
  filter(season == "20212022") %>% 
  arrange(desc(fantasyPoints))


# backman_df <- read.csv("Copy of Eddes DB - LastYear.csv")
# diff_df <- fantasy_df_clean_last_season %>% 
#   filter(fullName %in% setdiff(fantasy_df$fullName, backman_df$GOALIES))  %>% arrange(desc(fantasyPoints))
# diff_df
# fantasy_df_last_season %>% filter(grepl("Tim S", fullName))

write.table(fantasy_df, file = "dbs/fantasy_db.csv", row.names = FALSE)
write.table(fantasy_df_clean, file = "dbs/fantasy_db_clean.csv", row.names = FALSE)
write.table(fantasy_df_last_season, file = "dbs/fantasy_db_last_season.csv", row.names = FALSE)
write.table(fantasy_df_clean_last_season, file = "dbs/fantasy_db_clean_last_season.csv", row.names = FALSE)
