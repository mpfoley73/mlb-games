##
## Scrape data from Baseball Reference
## www.baseball-reference.com
##
## Note: I am replacing most of this data with the MLB API data collected in 
## 01_bet_mlb_data.R.
##

library(XML)
library(tidyverse)
library(lubridate)
library(glue)

bref_base_url <- "https://www.baseball-reference.com/teams/CLE"

# There is one web page per team per year. These functions have a built-in
# sleep time to comply with Baseball Reference's web scraping policy: 
# https://www.sports-reference.com/bot-traffic.html. 
# Policy currently limits traffic to 20/minute. Function set to 10/minute.
get_schedule_scores <- function(year) {
  u <- glue("{bref_base_url}/{year}-schedule-scores.shtml")
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
get_batting_pitching <- function(year) {
  u <- glue("{bref_base_url}/{year}.shtml")
  u_raw <- httr::GET(u)
  u_char <- rawToChar(u_raw$content)
  # Reads 2 tables: team_batting and team_pitching
  u_tbl <- readHTMLTable(u_char)
  # Limit to 10/minute
  Sys.sleep(6)
  u_tbl
}

# This script writes results to .rds for aggregation in later step. No need to
# pull any season more than one time. First time I ran this was with 20 seasons.
season <- 2023:2023
names(season) <- season

cle_games_raw <- map_df(season, ~get_schedule_scores(.x), .id = "season")
colnames(cle_games_raw) <- c(
  "season", "game_no", "date_str", "boxscore", "team", "home_ind", "opponent", 
  "outcome", "runs_scored", "runs_allowed", "innings", "new_record", "new_rank", 
  "new_games_back", "winning_pitcher", "losing_pitcher", "save", 
  "game_duration", "day_night", "attendance", "new_streak", "orig_sched", "cli"
)

cle_players_raw <- map(season, ~get_batting_pitching(.x))
cle_batting_raw <- map(cle_players_raw, ~.$team_batting) %>% list_rbind(names_to = "season")
cle_pitching_raw <- map(cle_players_raw, ~.$team_pitching) %>% list_rbind(names_to = "season")

# Write raw data sets to disk.
if (length(season) > 1) {
  dt_range <- glue("{season[1]}_{season[length(season)]}")
} else {
  dt_range <- glue("{season[1]}")
}

fn <- glue("cle_games_{dt_range}_raw.rds")
saveRDS(cle_games_raw, file.path("data", "bref", fn))

fn <- glue("cle_batting_{dt_range}_raw.rds")
saveRDS(cle_batting_raw, file.path("data", "bref", fn))

fn <- glue("cle_pitching_{dt_range}_raw.rds")
saveRDS(cle_pitching_raw, file.path("data", "bref", fn))

# Aggregate all the raw data sets into a single data frame and clean.
fns <- list.files(path = "data/bref", pattern = "^cle_games_.*_raw.rds$")
cle_games_agg_raw <- map_df(fns, ~readRDS(file.path("data", .x)))

fns <- list.files(path = "data/bref", pattern = "^cle_batting_.*_raw.rds")
cle_batting_agg_raw <- map_df(fns, ~readRDS(file.path("data", .x)))

fns <- list.files(path = "data/bref", pattern = "^cle_pitching_.*_raw.rds")
cle_pitching_agg_raw <- map_df(fns, ~readRDS(file.path("data", .x)))

# Clean the data
cle_games <- cle_games_agg_raw %>% 
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
    game_duration = as.duration(hm(game_duration, quiet = TRUE)),
    attendance = str_remove(attendance, ",")
  ) %>%
  # as.numeric throws a warning for conversion of empty strings.
  mutate(across(c(season, game_no, runs_scored:innings, new_rank,
                  new_games_back, attendance, cli), as.numeric)) %>%
  # Convert new_ cols into values entering the game
  group_by(season) %>%
  arrange(season, game_no) %>%
  mutate(
    old_record = lag(new_record, 1, default = "0-0"),
    old_rank = lag(new_rank, 1, default = 1),
    old_games_back = lag(new_games_back, 1, default = 0),
    old_streak = lag(new_streak, 1, default = 0)
  ) %>%
  ungroup() %>%
  relocate(starts_with("old_"), .after = 11) %>%
  relocate(new_streak, .after = 18) %>% 
  # innings only populated if <> 9
  replace_na(list(innings = 9)) %>%
  # outcome is W|L|T plus extra info like walk-off. Drop the extra info.
  mutate(outcome = factor(str_sub(outcome, 1, 1))) %>%
  select(
    season, game_no, game_date, opponent, outcome, runs_scored, runs_allowed, 
    innings, dur = game_duration, attendance, home_ind, day_ind, doubleheader_game,
    starts_with("old_"), starts_with("new_"), winning_pitcher, losing_pitcher, save
  )

cle_batting <- cle_batting_agg_raw %>% 
  # filter out repeating header rows
  filter(Rk != "Rk") %>%
  mutate(
    # Remove the symbols appended to name (*:=lefty, #:=switch, ?:=unknown).
    Name = str_remove_all(Name, "(\\*)|(#)|(\\?)"),
  ) %>%
  mutate(across(c(season, Age:IBB), as.numeric)) %>%
  select(-Rk)

cle_pitching <- cle_pitching_agg_raw %>%
  # filter out repeating header rows
  filter(Rk != "Rk") %>%
  mutate(
    # symbols appended to name: *:=lefty, #:=switch, ?:=unknown. Remove all.
    Name = str_remove_all(Name, "(\\*)|(#)|(\\?)"),
    ERA = if_else(ERA == "inf", "", ERA)
  ) %>%
  mutate(across(c(season, Age:`SO/W`), as.numeric)) %>%
  rename(SO_per_BB = `SO/W`) %>%
  select(-Rk)

# Save final data frames.
#
saveRDS(cle_games, file.path("data", "cle_games.rds"))

saveRDS(cle_batting, file.path("data", "cle_batting.rds"))

saveRDS(cle_pitching, file.path("data", "cle_pitching.rds"))
