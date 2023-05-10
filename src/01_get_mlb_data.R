##
## Pull data from MLB's stats API.
## statsapi.mlb.com
##
## This code is essentially copied from the baseballr package
## https://github.com/BillPetti/baseballr/blob/master/R/mlb_game_pace.R
## because I wasn't able to get it to work and wanted to see the internals
## anyway.
##

library(tidyverse)
library(lubridate)
library(glue)

module <- "gamePace"
mlb_base_url <- "http://statsapi.mlb.com/api/v1/gameTypes"

# This function pulls data from the API. There are modules(?) that you can
# query. Game duration is module "gamePace". Each module accepts a list of
# query parameters. 
get_mlb_data <- function(module, query_list = NULL) {
  u <- httr::modify_url(
    url = glue("http://statsapi.mlb.com/api/v1/{module}"), 
    query = query_list
  )
  
  raw_resp <- httr::RETRY("GET", u)

  resp_json <- rawToChar(raw_resp$content) %>%
    jsonlite::fromJSON(simplifyVector = TRUE, flatten = TRUE)
  
  # Limit to 10/minute
  Sys.sleep(6)

  resp_json
}

# get_mlb_data("sports") # MLB is `sportIds = 1`
# get_mlb_data("teams", list(sportIds = 1)) # Guardians are `teamIds = 114`
# get_mlb_data("gameTypes") # Regular season games are `gameType = R`
# get_mlb_data("gamePace", list(season = 2023, sportIds = 1, gameType = "R")) %>% pluck("sports")
# get_mlb_data("gamePace", list(season = 2023, teamIds = 114)) %>% pluck("teams")
# get_mlb_data("teams/stats", list(season = 1903, sportIds = 1, gameType = "R", stats = "season", group = "pitching")) %>% pluck("stats", "splits")

# This script writes results to .rds for aggregation in later step. No need to
# pull any season more than one time. First time I ran this was with 20 seasons.
season <- 2023

mlb_pace_raw <- map_df(
  season,
  ~get_mlb_data("gamePace", list(season = .x, sportIds = 1, gameType = "R")) %>%
    pluck("sports")
)

mlb_batting_raw <- map_df(
  season, 
  ~get_mlb_data("teams/stats", list(season = .x  , sportIds = 1, gameType = "R", stats = "season", group = "hitting")) %>% 
    pluck("stats", "splits")
)

mlb_pitching_raw <- map_df(
  season, 
  ~get_mlb_data("teams/stats", list(season = .x, sportIds = 1, gameType = "R", stats = "season", group = "pitching")) %>% 
    pluck("stats", "splits")
)

# Write raw data to disk.
if (length(season) > 1) {
  dt_range <- glue("{season[1]}_{season[length(season)]}")
} else {
  dt_range <- glue("{season[1]}")
}

fn <- glue("mlb_pace_{dt_range}_raw.rds")
saveRDS(mlb_pace_raw, file.path("data", "mlb", fn))
fn <- glue("mlb_batting_{dt_range}_raw.rds")
saveRDS(mlb_batting_raw, file.path("data", "mlb", fn))
fn <- glue("mlb_pitching_{dt_range}_raw.rds")
saveRDS(mlb_pitching_raw, file.path("data", "mlb", fn))

# Aggregate all the raw data sets into a single data frame and clean.
fns <- list.files(path = "data/mlb", pattern = "^mlb_pace_.*_raw.rds$")
mlb_pace_agg_raw <- map_df(fns, ~readRDS(file.path("data/mlb", .x)))
fns <- list.files(path = "data/mlb", pattern = "^mlb_batting_.*_raw.rds$")
mlb_batting_agg_raw <- map_df(fns, ~readRDS(file.path("data/mlb", .x)))
fns <- list.files(path = "data/mlb", pattern = "^mlb_pitching_.*_raw.rds$")
mlb_pitching_agg_raw <- map_df(fns, ~readRDS(file.path("data/mlb", .x)))

# Clean the data
mlb_pace <- mlb_pace_agg_raw %>%
  janitor::clean_names("snake") %>%
  mutate(
    season = as.numeric(season),
    dur = as.duration(hms(total_game_time)),
    extra_I_dur = as.duration(hms(total_extra_inn_time))
  ) %>%
  select(
    season, dur, extra_I_dur, G = total_games,
    extra_I_G = total_extra_inn_games,
    I = total_innings_played, H = total_hits,
    R = total_runs, PA = total_plate_appearances,
    pitchers = total_pitchers, P = total_pitches
  )

mlb_batting <- mlb_batting_agg_raw %>%
  janitor::clean_names("snake") %>%
  mutate(
    season = as.numeric(season),
    across(where(is.character) & starts_with("stat"), as.numeric)
  ) %>% 
  select(
    team = team_name, team_id, season, G = stat_games_played, R = stat_runs, 
    `2B` = stat_doubles, `3B` = stat_triples, HR = stat_home_runs, 
    BB = stat_base_on_balls, H = stat_hits, HBP = stat_hit_by_pitch, 
    BA = stat_avg, AB = stat_at_bats, SLG = stat_slg, OPS = stat_ops, 
    SB = stat_stolen_bases, CS = stat_caught_stealing, 
    PA = stat_plate_appearances, OBP = stat_obp, TB = stat_total_bases,
    RBI = stat_rbi, sac_bunts = stat_sac_bunts, pitches = stat_number_of_pitches,
    K = stat_strike_outs
  )

mlb_pitching <- mlb_pitching_agg_raw %>%
  janitor::clean_names("snake") %>% 
  mutate(
    season = as.numeric(season),
    across(where(is.character) & starts_with("stat"), ~if_else(str_detect(., "--"), NA_character_, .)),
    across(where(is.character) & starts_with("stat"), as.numeric)
  ) %>% 
  select(
    team = team_name, team_id, season, G = stat_games_played, 
    GS = stat_games_started, R = stat_runs, HR = stat_home_runs, 
    K = stat_strike_outs, BB = stat_base_on_balls, H = stat_hits, 
    HBP = stat_hit_by_pitch, BA = stat_avg, AB = stat_at_bats,
    OBP = stat_obp, ERA = stat_era, IP = stat_innings_pitched,
    W = stat_wins, L = stat_losses, SV = stat_saves, ER = stat_earned_runs, 
    WHIP = stat_whip, BF = stat_batters_faced, outs = stat_outs, 
    games_pitched = stat_games_pitched, CG = stat_complete_games, 
    shutouts = stat_shutouts, balks = stat_balks,
    wild_pitches = stat_wild_pitches, games_finished = stat_games_finished
  )

# Save final data frame.
saveRDS(mlb_pace, file.path("data", "mlb_pace.rds"))
saveRDS(mlb_batting, file.path("data", "mlb_batting.rds"))
saveRDS(mlb_pitching, file.path("data", "mlb_pitching.rds"))
