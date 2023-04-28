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

# You can use the API to look-up your query parameters!
# MLB is `sportIds = 1`
get_mlb_data("sports")
# The Guardians are `teamIds = 114`
get_mlb_data("teams", list(sportIds = 1))
# Regular season games are `gameType = R`
get_mlb_data("gameTypes")
# Pace of play
get_mlb_data("gamePace", list(season = 2023, sportIds = 1, gameType = "R")) %>% pluck("sports")
get_mlb_data("gamePace", list(season = 2023, teamIds = 114)) %>% pluck("teams")

# This script writes results to .rds for aggregation in later step. No need to
# pull any season more than one time. First time I ran this was with 20 seasons.
season <- 2023

mlb_pace_raw <- map_df(
  season, 
  ~get_mlb_data("gamePace", list(season = .x, sportIds = 1, gameType = "R")) %>% 
    pluck("sports"),
)

# Write raw data set to disk.
if (length(season) > 1) {
  dt_range <- glue("{season[1]}_{season[length(season)]}")
} else {
  dt_range <- glue("{season[1]}")
}

fn <- glue("mlb_pace_{dt_range}_raw.rds")
saveRDS(mlb_pace_raw, file.path("data", fn))

# Aggregate all the raw data sets into a single data frame and clean.
fns <- list.files(path = "data", pattern = "^mlb_pace_.*_raw.rds$")
mlb_pace_agg_raw <- map_df(fns, ~readRDS(file.path("data", .x)))

# Clean the data
mlb_pace <- mlb_pace_agg_raw %>%
  mutate(
    season = as.numeric(season),
    pitchers_per_9 = totalPitchers / totalInningsPlayed * 9,
    P_per_PA = totalPitches / totalPlateAppearances,
    time_per_9 = hms(timePer9Inn, quiet = TRUE), 
    time_per_PA = hms(timePerPlateAppearance, quiet = TRUE), 
    time_per_P = hms(timePerPitch, quiet = TRUE)
  ) %>%
  select(
    season, games = totalGames, H_per_9 = hitsPer9Inn, R_per_9 = runsPer9Inn, 
    PA_per_9 = plateAppearancesPer9Inn, P_per_9 = pitchesPer9Inn, pitchers_per_9, 
    P_per_pitcher = pitchesPerPitcher, P_per_PA, time_per_9, time_per_PA, time_per_P
  )

mlb_pace %>% str()

# Save final data frame.
saveRDS(mlb_pace, file.path("data", "mlb_pace.rds"))
