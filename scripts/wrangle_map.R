library(httr)
library(jsonlite)
library(rvest)
library(sf)
library(leaflet)
library(tidyverse)

response <- GET('https://api.openf1.org/v1/meetings')
meetings_api <- fromJSON(content(response, 'text')) |>
  select(-gmt_offset)


circuits_raw <- read_csv('raw-data/circuits.csv') |>
  select(-url, -alt) 


races_raw <- read_csv('raw-data/races.csv')
races_per_circuit <- races_raw |>
  select(circuitId, name) |>
  group_by(circuitId, name) |>
  summarize(num_races = n()) 

circuits <- circuits_raw |>
  left_join(races_per_circuit, by = 'circuitId')


# Leaflet prep

world_map_sf <- maps::map("world", plot = FALSE, fill = TRUE) |> 
  st_as_sf()


save(circuits, file = "data/circuits.RData")

# LEAFLET PLOT (to be put in code chunk of blog?)

# Add custom pin marker : https://r-charts.com/spatial/interactive-maps-leaflet/ (source)
icons_list <- icons(iconUrl = "https://raw.githubusercontent.com/R-CoderDotCom/chinchet/main/inst/red.png",
                    iconWidth = c(50, 60, 40), 
                    iconHeight = c(50, 60, 40))

leaflet(circuits) |> 
  addTiles() |>
  addMarkers(lng = ~lng,
             lat = ~lat,
             popup = ~country,
             icon = icons_list) |>
  # labels: stackoverflow citation
  addLabelOnlyMarkers(lng= ~lng,
                      lat = ~lat,
                      label = ~location,
                      labelOptions = labelOptions(
                        noHide = TRUE,
                        textOnly = TRUE,
                        direction = "bottom",
                        offset = c(0,5))) |>
  addMiniMap(width = 150, height = 150) |>
  setView(lat = 33.5786,
          lng = -7.6875,
          zoom = 2) 


# # Scrape coordinates from Wikos
# 
# n_iter <- 1
# circuit <- circuits$name
# urls <- circuits$url
# 
# circuit_locations <-
#   tibble(circuits = circuit, link = urls,
#          # Create empty character vector the same length as our titles vector
#          lat = vector(mode = "character", length = length(circuits)),
#          lng = vector(mode = "character", length = length(circuits)),
#          # Only need to specify argument names if they are not in order
#          illustration = vector("character", length(circuits))) |>  # Limit to only the rows we are trying to work with
#   slice(1:n_iter)
# 
# for (i in 1:n_iter) {
#   # Scrape nursery rhyme i's text and save it to row i of the text variable
#   circuit_locations$lat[i] <-
#     circuit_locations$link[i] |>
#     read_html() |>
#     html_elements(".latitude") |>
#     html_text()
#   
#   circuit_locations$lng[i] <-
#     circuit_locations$link[i] |>
#     read_html() |>
#     html_elements(".longitude") |>
#     html_text()
#   
# }
