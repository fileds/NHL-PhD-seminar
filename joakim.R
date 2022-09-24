library(dplyr)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(tidyverse)

df <- read.table("dbs/fantasy_db.csv", header = T)
df <- mutate(df, fantasyPerGame = fantasyPoints / games)

unique(df$position)
hist(filter(df, position == "W")$fantasyPoints)
hist(filter(df, position == "C")$fantasyPoints)
hist(filter(df, position == "D")$fantasyPoints)
hist(filter(df, position == "G")$fantasyPoints)

expsmooth <- function(data, id_col = "id", column_to_forecast = "fantasyPoints", forecast_fun = holt) {
  ids <- unique(data[, id_col])
  predictions <- vector("numeric", length(ids))
  for (i in 1:length(ids)) {
    fpoints <- df[df[, id_col] == ids[i], column_to_forecast]
    if (length(fpoints) == 1) {
      predictions[i] <- fpoints
    }
    else {
      pts <- ts(fpoints)
      predictions[i] <- forecast_fun(pts, h = 1)$mean[1]
    }
  }
  predictions
}


# Forecast fantasy points -------------------------------------------------
preds <- expsmooth(df)

# Predict number of games using trend and no trend
game_preds2 <- expsmooth(df, column_to_forecast = "games", forecast_fun = holt)
game_preds <- expsmooth(df, column_to_forecast = "games", forecast_fun = ses)
# Predict fantasy points/game using trend and no trend
fp_game_preds2 <- expsmooth(df, column_to_forecast = "fantasyPerGame", forecast_fun = ses)
fp_game_preds <- expsmooth(df, column_to_forecast = "fantasyPerGame")

# multiply the result, use trend for fantasy and no trend for games
preds2 <- game_preds*fp_game_preds
# They have 0 diff in a lot of cases
preds2-preds
# Add predictions to new data frame
newdf <- df[!duplicated(df[, c(1, 4)]), c(1, 3, 4)]
newdf$predFantasy <- preds

# Get good selection order ------------------------------------------------
players <- 30 # no of players
# decrease prob for forwards as we move on (use from real data later)
pick_probs <- rbind(data.frame(W = rep(0.4, 5),
                               C = rep(0.4, 5),
                               D = rep(0.1, 5),
                               G = rep(0.1, 5)),
                    data.frame(W = rep(0.3, 10),
                               C = rep(0.3, 10),
                               D = rep(0.3, 10),
                               G = rep(0.1, 10)),
                    data.frame(W = rep(0.2, 5),
                               C = rep(0.1, 5),
                               D = rep(0.4, 5),
                               G = rep(0.3, 5)))

pickings <- c(rep("W", 4), rep("C", 6), rep("D", 5), rep("G", 2), rep("B", 3))

iter <- 1000
iter <- 100
set.seed(1223)
team_list <- list()
score_vec <- vector("numeric", iter)
for (i in 1:iter) {
  if (i%%20 == 0) { print(i) }
  # sample random pick order
  # no_in_pick <- sample(1:players, 1) # we are the 9th person to pick
  pick_order <- sample(pickings)
  pick_df <- arrange(newdf, position, desc(predFantasy))
  C <- 1
  D <- min(which(pick_df$position == "D"))
  G <- min(which(pick_df$position == "G"))
  W <- min(which(pick_df$position == "W"))
  team <- pick_df[0, ] # store team here
  for (turn in 1:length(pickings)) {
    # random pos if we have a B (could be improved)
    pick_pos <- ifelse(pick_order[turn] == "B", 
                       sample(c("W", "C", "D", "G"), 1), pick_order[turn])
    if (pick_pos == "C") {
      D <- D-1
      G <- G-1
      W <- W-1
    } else if (pick_pos == "D") {
      G <- G-1
      W <- W-1
    } else if (pick_pos == "G") {
      W <- W-1
    }
    
    pick <- pick_df %>% 
      filter(position == pick_pos) %>%
      filter(predFantasy == max(predFantasy))
    team <- rbind(team, pick[1, ]) # add to team (first if duplicate)
    pick_df <- filter(pick_df, id != pick[1, ]$id) # remove new pick
    
    
    # remove picks from other people based on pick_probs (real data later)
    # below is complicated due to speed
    rem_C <- pick_probs[turn, ]$C*players # compute number to remove for each pos
    rem_D <- pick_probs[turn, ]$D*players
    rem_G <- pick_probs[turn, ]$G*players
    rem_W <- pick_probs[turn, ]$W*players
    old_df <- pick_df
    pick_df <- pick_df[-c(C:(C+rem_C-1), # remove picked players
                          D:(D+rem_D-1),
                          G:(G+rem_G-1),
                          W:(W+rem_W-1)), ]
    W <- W-rem_C-rem_D-rem_G
    G <- G-rem_C-rem_D
    D <- D-rem_C # adjust indices for next turn
  }
  team_list[[i]] <- team
  score_vec[i] <- sum(team$predFantasy)
}
which(score_vec == max(score_vec)) # best out of the 100
team_list[[18]]
which(score_vec[-18] == max(score_vec[-18]))
team_list[[1]]
which(score_vec[-c(1, 18)] == max(score_vec[-c(1, 18)]))
team_list[[50]]
