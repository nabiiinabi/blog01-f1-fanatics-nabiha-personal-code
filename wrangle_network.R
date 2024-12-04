# Load packages and raw data
library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(ggnetwork)
library(kableExtra)
library(gt)
options(scipen = 999)

# Document by: Sam Ramirez - Fall 2024

# Wrangle data ===============================================================

circuits <- read_csv("raw-data/circuits.csv") |> #cleaning circuits file
  janitor::clean_names() |>
  select(circuit_id, name, location, country) |>
  drop_na()

races <- read_csv("raw-data/races.csv") |> #cleaning races file
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

# Using date for chronological order
prix_by_year <- prix_by_id |>
  arrange(year, date) |>
  mutate(start = circuit_name,
         end = lead(circuit_name)) |>
  drop_na(end) |>
  group_by(start, end) |>
  summarise(total_visits = n()) 



# Using name and location only for identification
prix_by_name <- prix_by_id |>
  select(circuit_id, circuit_name, location, country) |>
  unique()

# Deleting rows that contain duplicate id's because of different 
# prix names within same country. Deleted rows are still counted, 
# only consolidated under one name
prix_by_name <- 
  prix_by_name[-c(4, 11, 20, 22, 24, 26, 27, 28, 30, 34, 42, 49, 53, 83, 86), ]

# SHOUTOUT PROF. ALFELD RAHHHHHH ------------------------------------
# I spent so much time trying to figure this out and no one will ever 
# know because I have to delete all the commented out code :sob:
next_circuit <- data.frame(
  
  # lead() within mutate <-------------
  
  
  
  start = prix_by_year$circuit_id[-length(prix_by_year$circuit_id)],# All but the last element
  end = prix_by_year$circuit_id[-1] # All but the first element
  
  ) |>
  group_by(start, end) |>
  summarise(total_visits = n()) 


# |
#   inner_join(prix_by_name, 
#             by = join_by("start" == "circuit_id"), 
#             suffix = c(".x", ".y"),
#             relationship = "many-to-many") |>
#   inner_join(prix_by_name, 
#             by = join_by("end" == "circuit_id"), 
#             suffix = c(".x", ".y"),
#             relationship = "many-to-many") 



# Circuit data as tibble for networks... when does this end ----------

circuit_graph <- as_tbl_graph(prix_by_year, directed = TRUE)
circuit_edges <- with_graph(circuit_graph, .E())
circuit_nodes <- with_graph(circuit_graph, .N())


circuit_graph <- circuit_graph |>
  activate(nodes) |>
    left_join(prix_by_name,
              by = join_by("name" == "circuit_name"))
  




plot(circuit_graph)

ggraph(circuit_graph) +
geom_edge_fan(edge_linetype = 3, edge_alpha = 0.5,
                  end_cap = circle(10, "pt"),
                  arrow = arrow(type = "closed", angle = 10,
                                length = unit(15, "pt"))) +
geom_node_point(size = 5) +
geom_node_label(aes(label = name), repel = TRUE) +
# Had to add `base_family` argument due to error with fonts loading for PDF
theme_graph(base_family = "sans")




# |
#   pivot_wider(names_from = date, 
#               values_from = circuit_id)

# list <- for(x in 1:77){
#   for(i in 1:77)
#     {
#       if(prix_by_year[x, 1] == TRUE){
#         prevNode <- prix_by_year[x,i]
#       }
#     }
#   }

# next_circuit <- list()
# 
# for(target in 1:length(prix_by_year$circuit_id) - 1){
#   for(i in 1:77){
#     if(prix_by_year$circuit_id == target){
#       next_circuit <- c(next_circuit, numbers[i+1])
#       }
#   }
# }

# need to store the next node from each circuit
# for example, for circuit 9, I need to get the circuit that 
# follows for each time that 9 appears (9 -> 20, 9 -> 6...)

#============================= FROM CHAPTER 20 MEN'S BASKET (doent work :())

# prefix <- "https://www.kaggle.com/c/march-machine-learning-mania-2015"
# url_teams <- paste(prefix, "download/teams.csv", sep = "/")
# url_games <- paste(
#   prefix, 
#   "download/regular_season_compact_results.csv", sep = "/"
# )
# download.file(url_teams, destfile = "data/teams.csv")
# download.file(url_games, destfile = "data/games.csv")
# 
# 
# E <- games |>
#   mutate(score_ratio = wscore/lscore) |>
#   select(lteam, wteam, score_ratio)
# V <- teams |>
#   filter(team_id %in% unique(c(E$lteam, E$wteam)))
# 
# g <- igraph::graph_from_data_frame(E, directed = TRUE, vertices = V) |>
#   as_tbl_graph() |>
#   mutate(team_id = parse_number(name))
# summary(g)



#=============================




# Save data sets ========================================

save([...], file = "network_data.RData")

  
  
  
  
  