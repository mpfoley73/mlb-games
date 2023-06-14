##
## Aggregate event files from Retrosheet
## https://www.retrosheet.org/game.htm
##
## This script has three sections:
## 1. Download game logs from Retrosheet
## 2. Run Chadwick to create csv files.
## 3. Insert into MySQL database.
## 4. Post-processing

library(tidyverse)
library(odbc)
library(glue)
library(readxl)

# Setup ------------------------------------------------------------------------
# Seasons to download and process. Retrosheet explains that all seasons are
# subject to change (https://www.retrosheet.org/game.htm#Notice). Might be worth
# a re-run. Currently available: 1914-2022.
chadwick_dir <- "C:\\Users\\mpfol\\OneDrive\\Documents\\GitHub\\mlb-games\\assets\\chadwick"
retro_yrs <- 1941:2022
wd <- getwd()

# 1. Download game logs from Retrosheet ----------------------------------------
event_dir <- file.path(wd, "data/retrosheet/event")
setwd(event_dir)
for (season in retro_yrs) {
  zip_fn <- glue("{season}eve.zip")
  zip_path <- file.path(event_dir, zip_fn)
  download.file(url = glue("https://www.retrosheet.org/events/{zip_fn}"), destfile = zip_path)
  unzip(zipfile = zip_path, exdir = event_dir)
  shell(glue("del {zip_fn}"))
}

# 2. Run Chadwick to create csv files:
#  - event[YYYY].csv,
#  - sub[YYYY].csv,
#  - game[YYYY].csv,
#  - roster[YYYY].csv, and 
#  - team[YYYY].csv) 
# from the *.EV[AFN], *.ROS, and TEAM files.
# Note: Run `shell("cwevent -h")` for description of flags
setwd(event_dir)
roster_col_names <- c("PLAYER_ID", "LAST_NAME", "FIRST_NAME", "BATS", "THROWS", "TEAM_ID", "POS")
roster_col_types <- str_c("ccccccc")
team_col_names <- c("TEAM_ID", "LEAGUE_ID", "TEAM_CITY", "TEAM_NAME")
team_col_types <- c("cccc")
for (season in retro_yrs) {
  # Create event[YYYY].csv.
  cwevent_cmd <- glue("{file.path(chadwick_dir, 'cwevent')} -y {season} -f 0-96 -x 0-60 -q -n {season}*.EV* > event{season}.csv")
  shell(cwevent_cmd)
  # Create sub[YYYY].csv.
  cwsub_cmd <- glue("{file.path(chadwick_dir, 'cwsub')} -y {season} -q -n {season}*.EV* > sub{season}.csv")
  shell(cwsub_cmd)
  # Create game[YYYY].csv.
  cwgame_cmd <- glue("{file.path(chadwick_dir, 'cwgame')} -y {season} -f 0-83 -x 0-96 -q -n {season}*.EV* > game{season}.csv")
  shell(cwgame_cmd)
  # Create roster[YYYY].csv from individual team files. File names are like 
  # CLE1973.ROS, one for each team.
  roster_fn <- list.files(pattern = glue("\\.*{season}\\.ROS$"))
  roster_df <- map_df(roster_fn, ~read_csv(., col_names = roster_col_names, col_types = roster_col_types))
  data.table::fwrite(roster_df, glue("roster{season}.csv"))
  # Create team[YYYY].csv from TEAM[YYYY]. 
  team_fn <- glue("TEAM{season}")
  team_df <- read_csv(team_fn, col_names = team_col_names, col_types = team_col_types)
  data.table::fwrite(team_df, glue("team{season}.csv"))
  # Clean up
  shell(glue("del {season}*.EV*"))
  shell(glue("del *{season}.ROS"))
  shell(glue("del TEAM{season}"))
}

# 3. Insert into MySQL database.
# At this point you have one or more event[YYYY].csv, sub[YYYY].csv, 
# game[YYYY], roster[YYYY].csv, and team[YYYY].csv files. Read each file, add a 
# year_id col, and insert into MySQL.
setwd(event_dir)
retrosheet_conn <- dbConnect(
  RMySQL::MySQL(),
  dbname = "retrosheet",
  host = "localhost",
  port = 3306,
  user = "root",
  password = "password" 
)
event_col_types <- str_c(
  "ccdddddcddcccccccccccccccccccclldddlldlldlldlldcll",
  "cddcdcdcdddddccclllllllllccclllllccccdddddddddd", # 97 standard fields
  "cccdlldddddllddllcclldddddddddcccdddddddddddddclllldddddddddd" # 61 extended fields
)
sub_col_types <- str_c("cddcdddcdd")
game_col_types <- str_c(
  "cddcdlcccccccccclldcccclddddddddddddddddddccclcdcd",
  "cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcccclldlllccdddddd",
  "ddddddddddddddddddddddddddddddddddddddddddddllllll",
  "llllccccccccccccccccccccccclldl"
)
for (season in retro_yrs) {
  event_df  <- read_csv(glue("event{season}.csv"),  col_types = event_col_types)
  sub_df    <- read_csv(glue("sub{season}.csv"),    col_types = sub_col_types)
  game_df   <- read_csv(glue("game{season}.csv"),   col_types = game_col_types)
  roster_df <- read_csv(glue("roster{season}.csv"), col_types = roster_col_types)
  team_df   <- read_csv(glue("team{season}.csv"),   col_types = team_col_types)
  # Add year_id col and change column name case to lower.
  event_df  <- event_df  %>% janitor::clean_names("snake") %>% mutate(year_id = season) %>% select(game_id, event_id, year_id, everything())
  sub_df    <- sub_df    %>% janitor::clean_names("snake") %>% mutate(year_id = season) %>% select(game_id, event_id, year_id, everything())
  game_df   <- game_df   %>% janitor::clean_names("snake") %>% mutate(year_id = season) %>% select(game_id, year_id, everything())
  roster_df <- roster_df %>% janitor::clean_names("snake") %>% mutate(year_id = season) %>% select(team_id, year_id, player_id, everything())
  team_df   <- team_df   %>% janitor::clean_names("snake") %>% mutate(year_id = season) %>% select(team_id, year_id, everything())
  # Write to db
  dbWriteTable(retrosheet_conn, "game_event",  event_df,  append = TRUE, overwrite = FALSE, row.names = FALSE)
  dbWriteTable(retrosheet_conn, "player_sub",  sub_df,    append = TRUE, overwrite = FALSE, row.names = FALSE)
  dbWriteTable(retrosheet_conn, "game",        game_df,   append = TRUE, overwrite = FALSE, row.names = FALSE)
  dbWriteTable(retrosheet_conn, "team_roster", roster_df, append = TRUE, overwrite = FALSE, row.names = FALSE)
  dbWriteTable(retrosheet_conn, "team",        team_df,   append = TRUE, overwrite = FALSE, row.names = FALSE)
  print(glue("{season}: {scales::comma(nrow(event_df), 1)} event, ",
             "{scales::comma(nrow(sub_df), 1)} sub, ",
             "{scales::comma(nrow(roster_df), 1)} roster, ",
             "{scales::comma(nrow(game_df), 1)} game, ",
             "{scales::comma(nrow(team_df), 1)} team."))
  shell(glue("del event{season}.csv"))
  shell(glue("del sub{season}.csv"))
  shell(glue("del game{season}.csv"))
  shell(glue("del roster{season}.csv"))
  shell(glue("del team{season}.csv"))
}

setwd(wd)

# 4. Post-processing
# Address data issues. Some unknown values default to 0.
dbSendStatement(retrosheet_conn, "update game set minutes_game_ct = NULL where minutes_game_ct = 0")
dbSendStatement(retrosheet_conn, "update game set attend_park_ct  = NULL where attend_park_ct  = 0")
# Add lookups. Got this from https://chadwick.readthedocs.io/en/latest/cwevent.html.
lu_df <- read_xlsx(file.path(wd, "assets/chadwick/code_lookup.xlsx"))
colnames(lu_df) <- c("event_cd", "event_tx")
dbWriteTable(retrosheet_conn, "event_lu",  lu_df, overwrite = TRUE, append = FALSE, row.names = FALSE)
# Create indexes per recommendation from
# https://billpetti.github.io/2019-08-10-build-retrosheet-event-roster-database-rstats-baseballr/
dbSendStatement(retrosheet_conn, "alter table game add primary key (game_id(12))")
dbSendStatement(retrosheet_conn, "alter table game_event add primary key (game_id(12), event_id)")
dbSendStatement(retrosheet_conn, "alter table team add primary key (team_id(3), year_id)")
# Problem with team_roster PK: dup record.
dbSendStatement(retrosheet_conn, "delete from team_roster where team_id = 'OAK' and year_id = 2006 and player_id = 'kigem001' and pos = 'X'")
dbSendStatement(retrosheet_conn, "alter table team_roster add primary key (team_id(3), year_id, player_id(8))")
# (no unique key for player_sub)
dbSendStatement(retrosheet_conn, "create index sub_game_event on player_sub (game_id(12), event_id)")
dbSendStatement(retrosheet_conn, "alter table event_lu add primary key (event_cd)")
# Other indexes for performance
dbSendStatement(retrosheet_conn, 'create index game_year on game (year_id)')
dbSendStatement(retrosheet_conn, 'create index event_year on game_event (year_id)')
dbSendStatement(retrosheet_conn, 'create index roster_year on team_roster (year_id)')
dbSendStatement(retrosheet_conn, 'create index sub_year on player_sub (year_id)')
dbSendStatement(retrosheet_conn, 'create index team_year on team (year_id)')
dbSendStatement(retrosheet_conn, 'create index event_game_id on game_event (game_id(12))')
dbSendStatement(retrosheet_conn, 'create index event_home_team_id on game_event (home_team_id(5))')
dbSendStatement(retrosheet_conn, 'create index event_away_team_id on game_event (away_team_id(5))')
# dbSendStatement(retrosheet_conn, 'create index event_bat_id on event (bat_id(8))')
# dbSendStatement(retrosheet_conn, 'create index event_pit_id on event (pit_id(8))')
# dbSendStatement(retrosheet_conn, 'create index event_bat_hand_cd on event (bat_hand_cd)')
# dbSendStatement(retrosheet_conn, 'create index event_event_tx on event (event_tx)')
# dbSendStatement(retrosheet_conn, 'create index event_event_id on event (event_id)')
# dbSendStatement(retrosheet_conn, 'create index event_pit_hand_cd on event (pit_hand_cd)')
dbSendStatement(retrosheet_conn, 'create index event_event_cd on game_event (event_cd)')
# dbSendStatement(retrosheet_conn, 'create index event_event_outs_ct on event (event_outs_ct)')
# dbSendStatement(retrosheet_conn, 'create index event_inn_ct on event (inn_ct)')
