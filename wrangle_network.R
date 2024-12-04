# Load packages and raw data
library(tidyverse)
library(kableExtra)
library(gt)
library(glue)
options(scipen = 999)

# Wrangle data ===============================================================

# Sam Ramirez
circuits <- read_csv("raw-data/circuits.csv") |> 
  janitor::clean_names() |>
  select(circuit_id, name, location, country) |>
  drop_na()

races <- read_csv("raw-data/races.csv") |> 
  janitor::clean_names() |>
  select(year, circuit_id, name, date) |>
  drop_na() |>
  arrange(year) |>
  group_by(circuit_id) |>
  summarise(year = year,
            name = name,
            date = date)
 
# Join tables by circuit_id ==========================================

prix_by_id <- circuits |>
  left_join(races, by = "circuit_id", suffix = c(".x", ".y")) |>
  rename(circuit_name = name.x, prix_name = name.y) |>
  relocate(prix_name, .after = circuit_id)

# Change for networks ==========================================

prix_by_year <- prix_by_id |>
  arrange(date) |>
  select(circuit_id, year) |>
  pivot_wider(names_from = year, 
              values_from = circuit_id) |>
  separate_columns()



#=============================




#=============================




# Save data sets ========================================

save(prix_by_id,
     file = "network_data.RData")

  
  
  
  
  