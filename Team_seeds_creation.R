FinalSeed <- 'C:/Users/eorra/OneDrive/Documents/BS625 Project/ncaa_tourney_games.csv'
dataFS <- read.csv(FinalSeed)
View(dataFS)
library(dplyr)

team_seeds <- bind_rows(
  dataFS %>% 
    select(season, team_id = win_team_id, seed = win_seed, school_name = win_school_ncaa),
  dataFS %>% 
    select(season, team_id = lose_team_id, seed = lose_seed, school_name = lose_school_ncaa)
) %>%
  # Filter for seasons between 2000 and 2016
  filter(season >= 2000 & season <= 2016) %>%
  distinct() %>% 
  arrange(team_id, season)

# Save or display the results
print(team_seeds)

write.csv(team_seeds, "team_seeds_2000_2016.csv", row.names = FALSE)
write.csv(team_seeds, "C:/Users/eorra/OneDrive/Documents/BS625 Project/team_seeds_2000_2016.csv", row.names = FALSE)
