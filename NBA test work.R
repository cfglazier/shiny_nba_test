library(tidyverse)
library(plyr)
library(plotly)
source("helpers.R")
load("nba_shots.RData")

# store court plot 
gg_court = make_court()
gg_court

# summarize shots by player in dataset
nba_shots %>% 
  group_by(player_name) %>%
  summarize(n())

# transform data types for turning non-factors into factors, reclassify shot_zone_area
table(nba_shots[ , "action_type"])
table(nba_shots[ , "shot_zone_area"])

sapply(nba_shots, class)
transform(nba_shots, season = as.factor(season),
          shot_zone_area = as.factor(shot_zone_area),
          action_type = as.factor(action_type),
          period = as.factor(period))


nba_shots$shot_zone_area <- revalue(nba_shots$shot_zone_area, c("Back Court(BC)" = "BC", 
                                                                "Center(C)" = "C", 
                                                                "Left Side Center(LC)" = "LC", 
                                                                "Left Side(L)" = "L", 
                                                                "Right Side Center(RC)" = "RC", 
                                                                "Right Side(R)" = "R"))
levels(nba_shots$shot_zone_area)

# plot of where a specific NBA player's shots over a selected series
player_data = filter(nba_shots, player_name == 'LeBron James', season == "2003-04")
player_data1 = filter(nba_shots, player_name == 'LeBron James')

# plot the court, plot the player's shots, color them based on makes and misses 
gg_court + 
  geom_point(data = player_data1, alpha = 0.75, size = 2.5,
                      aes(loc_x, loc_y, color = shot_made_flag)) +
  scale_color_manual("", values = c(made = "blue", missed = "orange"))

# plot the court, plot the player's shots, color them based on shot types
gg_court + 
  geom_point(data = player_data1, alpha = 0.75, size = 2.5,
             aes(loc_x, loc_y, color = shot_zone_area)) +
  scale_color_manual("", values = c(BC = "blue",
                                    C = "green",
                                    LC = "yellow",
                                    L = "orange",
                                    RC = "red",
                                    R = "purple" ))
# boxplots of different player's shot distance
plot_ly(data = nba_shots, y = ~shot_distance, color = ~player_name, type = "box") %>% 
  layout(legend = list(x = 0.2, y = 1.0))

# boxplots of different distances from the zone area
plot_ly(data = player_data1, y = ~shot_distance, color = ~shot_zone_area, type = "box") %>% 
  layout(legend = list(x = 0.2, y = 1.0))
