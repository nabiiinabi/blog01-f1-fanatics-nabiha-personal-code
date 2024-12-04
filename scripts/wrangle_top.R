
library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)
library(stringr)
library(weathermetrics)
library(tmap)
library(plotly)


# TEAM (constructor) STANDINGS ---------------------

constructors_raw <- read_csv("raw-data/constructors.csv")
constructor_standings_raw <- read_csv("raw-data/constructor_standings.csv")
constructor_results_raw <- read_csv("raw-data/constructor_results.csv")

constructor_standings <- constructors_raw |>
  left_join(constructor_standings_raw, by = 'constructorId') |>
  select(-url, -nationality)

podiums <- constructor_standings |>
  filter(position %in% c(1, 2, 3)) |>
  group_by(constructorId, name) |>
  summarize(total_podiums = n(),
            total_wins = sum(position == 1, na.rm = TRUE)) |>
  arrange(desc(total_podiums), desc(total_wins)) |>
  head(10)

save(podiums, file = "data/team_podiums.RData")

# DRIVER STANDINGS --------------------------------


standings <- read_csv("raw-data/driver_standings.csv")

driver_standings_per_race <- standings |>
  left_join(drivers, by = 'driverId') |>
  left_join(races, by = 'raceId') |>
  select(-nationality, 
         -url,
         -ends_with("_date"),
         -ends_with("_time"))


# WEATHER VS LAP TIMES ? --------------------------

response_weather <- GET('https://api.openf1.org/v1/weather')
weather <- fromJSON(content(response_weather, 'text'))
# 
# response_sessions <- GET('https://api.openf1.org/v1/sessions')
# sessions <- fromJSON(content(response_sessions, 'text'))


response_meetings <- GET('https://api.openf1.org/v1/meetings')
meetings <- fromJSON(content(response_meetings, 'text')) |>
  select(-meeting_code)
races <- read_csv("raw-data/races.csv") 


race_weather <- weather |>
  left_join(meetings, by = 'meeting_key') |>
  filter(str_ends(meeting_name, "Grand Prix")) |>
  # created lambda func to convert Celsius to Fahrenheit
  mutate(across(ends_with("_temperature"), ~ .x * (9/5) + 32))

laps_raw <- read_csv("raw-data/lap_times.csv")


drivers <- read_csv("raw-data/drivers.csv") |>
  select(-number, -code, -url) |>
  rename(firstname = forename,
         lastname = surname)

