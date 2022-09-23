library(dplyr)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(tidyverse)
library(ggplot2)


df <- read.table("dbs/fantasy_db_clean_last_season.csv", header = TRUE)
