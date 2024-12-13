library(tidyverse)


ncaa_data = read_csv("ncaa_data.csv") %>% filter(season > 2000, current_division == "D1")
ncaa_teams = read_csv("ncaa_teams.csv")

ncaa_data = ncaa_data %>% select(season, team_code, points_game, win, opp_code, opp_points_game)
ncaa_teams = ncaa_teams %>% select(school_ncaa, team_code = code_ncaa, conf_name) 



data = left_join(ncaa_data, select(ncaa_teams, home_team = school_ncaa, team_code, home_conf = conf_name), by = c("team_code")) 
data = left_join(data, select(ncaa_teams, away_team = school_ncaa, opp_code = team_code, away_conf = conf_name), by = c("opp_code"))

#Some teams dropped from d1 to d2 or lower so dropped them
data = data %>% na.omit()

data = data %>% mutate(point_diff = points_game - opp_points_game, win = ifelse(win, 1, 0)) %>%
  select(season, home_team, away_team, result = win, home_conf, away_conf, point_diff)



library(BradleyTerry2)
data = data %>% filter(season == 2001)
# Example dataset
bt_model <- BTm(
  cbind(result, 1 - result),  # Outcome
  player1 = as.factor(home_team), 
  player2 = as.factor(away_team), 
  formula = ~ as.factor(home_conf) + as.factor(away_conf) + point_diff,
  data = data
)

summary(bt_model)




bt_model <- BTm(
  cbind(result, 1 - result),
  player1 = as.factor(home_team),
  player2 = as.factor(away_team),
  formula = ~ point_diff,
  data = data
)

