##
## Scrape Attendance Data
##

library(XML)
library(tidyverse)
library(lubridate)
library(glue)

# There is one web page per team per year. These functions have a built-in
# sleep time to comply with Baseball Reference's web scraping policy: 
# https://www.sports-reference.com/bot-traffic.html. 
# Policy currently limits traffic to 20/minute. Function set to 10/minute.
get_schedule_scores <- function(team, year) {
  u <- glue("https://www.baseball-reference.com/teams/{team}/{year}-schedule-scores.shtml")
  u_raw <- httr::GET(u)
  u_char <- rawToChar(u_raw$content)
  u_tbl <- readHTMLTable(u_char, which = 1)
  # 1901 and 1902 missing col "cLI" between Attendance and Streak
  if(year <= 1902) { u_tbl$cLI <- NA_character_ }
  # Limit to 10/minute
  Sys.sleep(6)
  u_tbl
}

# The /teams/CLE/YYYY.shtml page has several tables, but only the first two
# (batting and pitching) show up in the html (the others are commented out???).
# This function grabs both and returns them as a list.
get_batting_pitching <- function(team, year) {
  u <- glue("https://www.baseball-reference.com/teams/{team}/{year}.shtml")
  u_raw <- httr::GET(u)
  u_char <- rawToChar(u_raw$content)
  # Reads 2 tables: team_batting and team_pitching
  u_tbl <- readHTMLTable(u_char)
  # Limit to 10/minute
  Sys.sleep(6)
  u_tbl
}

# This script writes results to .rds, then reads existing rds files to create
# a single aggregate. No need to pull any season more than one time.
season <- 2023:2023
names(season) <- season

cle_games_raw <- map_df(season, ~get_schedule_scores("CLE", .x), .id = "season")
colnames(cle_games_raw) <- c(
  "season", "game_no", "date_str", "boxscore", "team", "home_ind", "opponent", 
  "outcome", "runs_scored", "runs_allowed", "innings", "new_record", "new_rank", 
  "new_games_back", "winning_pitcher", "losing_pitcher", "save", 
  "game_duration", "day_night", "attendance", "new_streak", "orig_sched", "cli"
)

cle_players_raw <- map(season, ~get_players("CLE", .x))
cle_batting_raw <- map(cle_players_raw, ~.$team_batting) %>% list_rbind(names_to = "season")
cle_pitching_raw <- map(cle_players_raw, ~.$team_pitching) %>% list_rbind(names_to = "season")

# Clean the data.
cle_games <- cle_games_raw %>% 
  # filter out repeating header rows
  filter(game_no != "Gm#") %>%
  # filter out future games
  filter(runs_scored != "Game Preview, and Matchups") %>%
  mutate(
    # if date_str like "Friday, Sep 29 (1)", then first game of a doubleheader
    doubleheader_game = factor(as.numeric(
      if_else(str_detect(date_str, "\\([:digit:]\\)"),
              str_sub(str_extract(date_str, "\\([:digit:]\\)"), 2, 2),
              "0"))),
    # from "[Weekday], Mmm d (#)", remove [Weekday] and (#) to make real date
    game_date = str_remove_all(date_str, "([:alpha:]*,)|(\\([:digit:]\\))"),
    game_date = mdy(paste(game_date, season)),
    home_ind = factor(if_else(home_ind != "@", "Home", "Away")),
    day_ind = factor(if_else(day_night == "D", "Day", "Night")),
    # new_games_back includes "+" when in first place, "Tied" when tied.
    new_games_back = case_when(
      new_games_back == "Tied" ~ "0",
      str_detect(new_games_back, "up ") ~ str_replace(new_games_back, "up ", "-"),
      str_detect(new_games_back, "up") ~ str_replace(new_games_back, "up", "-"),
      TRUE ~ new_games_back),
    # new_streak is of form "++++" or "----"
    new_streak = if_else(str_detect(new_streak, "-"), -1, 1) * str_length(new_streak),
    # quiet = TRUE because many some games have no recorded duration
    game_duration = hm(game_duration, quiet = TRUE),
    attendance = str_remove(attendance, ",")
  ) %>%
  mutate(across(c(season, game_no, runs_scored:innings, new_rank,
                  new_games_back, attendance, cli), as.numeric)) %>%
  # Convert new_ cols into values entering the game
  group_by(season) %>%
  arrange(season, game_no) %>%
  mutate(
    record = lag(new_record, 1, default = "0-0"),
    rank = lag(new_rank, 1, default = 1),
    games_back = lag(new_games_back, 1, default = 0),
    streak = lag(new_streak, 1, default = 0)
  ) %>%
  ungroup() %>%
  # innings only populated if <> 9
  replace_na(list(innings = 9)) %>%
  # outcome is W|L|T plus extra info like walk-off. Drop the extra info.
  mutate(outcome = factor(str_sub(outcome, 1, 1))) %>%
  select(-c(boxscore, date_str, starts_with("new_"), day_night)) %>%
  select(season:team, opponent, game_date:streak, doubleheader_game, everything()) 

cle_batting <- cle_batting_raw %>% 
  # filter out repeating header rows
  filter(Rk != "Rk") %>%
  mutate(
    # symbols appended to name: *:=lefty, #:=switch, ?:=unknown. Remove all.
    Name = str_remove_all(Name, "(\\*)|(#)|(\\?)"),
  ) %>%
  mutate(across(c(season, Age:IBB), as.numeric)) %>%
  select(-Rk)

cle_pitching <- cle_pitching_raw %>% 
  # filter out repeating header rows
  filter(Rk != "Rk") %>%
  mutate(
    # symbols appended to name: *:=lefty, #:=switch, ?:=unknown. Remove all.
    Name = str_remove_all(Name, "(\\*)|(#)|(\\?)"),
  ) %>%
  mutate(across(c(season, Age:SO9), as.numeric)) %>%
  select(-Rk)

# Write data sets to disk.
# fn <- glue("cle_games_{season[1]}_{season[length(season)]}.rds")
fn <- glue("cle_games_{season[1]}.rds")
saveRDS(cle_games, file.path("data", fn))

# fn <- glue("cle_batting_{season[1]}_{season[length(season)]}.rds")
fn <- glue("cle_batting_{season[1]}.rds")
saveRDS(cle_batting, file.path("data", fn))

# fn <- glue("cle_pitching_{season[1]}_{season[length(season)]}.rds")
fn <- glue("cle_pitching_{season[1]}.rds")
saveRDS(cle_pitching, file.path("data", fn))

# Combine the historical files into a single file.
fns <- list.files(path = "data", pattern = "^cle_games_[0-9]+.*.rds")
cle_games_agg <- map_df(fns, ~readRDS(file.path("data", .x)))
saveRDS(cle_games_agg, file.path("data", "cle_games.rds"))

fns <- list.files(path = "data", pattern = "^cle_batting_[0-9]+.*.rds")
cle_batting_agg <- map_df(fns, ~readRDS(file.path("data", .x)))
saveRDS(cle_batting_agg, file.path("data", "cle_batting.rds"))

fns <- list.files(path = "data", pattern = "^cle_pitching_[0-9]+.*.rds")
cle_pitching_agg <- map_df(fns, ~readRDS(file.path("data", .x)))
saveRDS(cle_pitching_agg, file.path("data", "cle_pitching.rds"))
