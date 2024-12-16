# Load library
library(tidyverse)

# Read and preprocess game data
ncaa_data = read_csv("ncaa_data.csv") %>%
  filter(season >= 2000, current_division == "D1") # Filter data for Division 1 teams from seasons after 2000

# Read teams data
ncaa_teams = read_csv("ncaa_teams.csv")

# Read team seeds data and select relevant columns
seeds = read_csv("team_seeds_2000_2016.csv") %>%
  select(season, school_name, seed)

# Define a function to process data and fit a BT model for a given season
process_model = function(ncaa, teams, seeds, year){
  
  # Filter and select relevant columns for given season
  ncaa = ncaa %>% select(season, team_code, points_game, win, opp_code, opp_points_game) %>% 
    filter(season == year)
  
  # Select relevant columns from teams data
  teams = teams %>% select(school_ncaa, team_code = code_ncaa, conf_name) 
  
  # Merge data with home team information
  data = left_join(ncaa, select(teams, home_team = school_ncaa, team_code, home_conf = conf_name), by = c("team_code")) 
  
  # Merge data with away team information
  data = left_join(data, select(teams, away_team = school_ncaa, opp_code = team_code, away_conf = conf_name), by = c("opp_code"))
  
  # Remove rows with missing data (like teams that dropped divisions)
  data = data %>% na.omit()
  
  # Create new variables for point difference and binary win/loss outcome
  data = data %>% mutate(point_diff = points_game - opp_points_game, win = ifelse(win, 1, 0)) %>%
    select(season, home_team, away_team, result = win, home_conf, away_conf, point_diff)
  
  # Count number of games for each team as home and away
  home_counts <- table(data$home_team)
  away_counts <- table(data$away_team)
  
  # Choose teams with more than 5 home games
  valid_teams <- names(home_counts[home_counts > 5])
  
  # Filter data to include only valid teams to reduce bias
  filtered_data <- subset(data, home_team %in% valid_teams & away_team %in% valid_teams)
  
  # Reshape data for logistic regression
  data_long <- filtered_data %>%
    pivot_longer(cols = c(home_team, away_team), 
                 names_to = "location", 
                 values_to = "team") %>%
    mutate(home = ifelse(location == "home_team", 1, 0),  # Indicate if game was played at home
           result = ifelse(location == "home_team", result, 1 - result)) # Adjust result for away games
  
  print(paste0("Fitting model for season ", year)) # Indicate which season's model is being processed
  
  # Fit Bradley-Terry model using logistic regression
  bt_model <- glm(
    result ~ home + abs(point_diff) + as.factor(team) + as.factor(home_conf) + as.factor(away_conf) - 1,
    family = binomial(link = "logit"),
    data = data_long
  )
  
  # Extract team strength coefficients
  team_coefficients <- coef(bt_model)[grep("^as.factor\\(team\\)", names(coef(bt_model)))]
  names(team_coefficients) <- gsub("as.factor\\(team\\)", "", names(team_coefficients))
  ranked_teams <- sort(team_coefficients, decreasing = TRUE)
  
  # Create a dataframe of ranked teams
  ranked_teams <- data.frame(
    Team = names(ranked_teams),
    Strength = ranked_teams
  )
  
  # Select top 64 teams and assign seeds
  top_64 = ranked_teams[1:64,] %>% mutate(season = year)
  top_64$school_name = rownames(top_64)
  top_64$bt_seed <- rep(1:16, each = 4) # Assign seeds (1-16) in groups of 4
  
  # Merge with actual seeds for comparison
  ranking = left_join(top_64, seeds, by = c("school_name", "season"))
  
  summary_model <- summary(bt_model)
  coefficients <- data.frame(
    variable = rownames(summary_model$coefficients),
    estimate = summary_model$coefficients[, "Estimate"],
    std_error = summary_model$coefficients[, "Std. Error"],
    z_value = summary_model$coefficients[, "z value"],
    p_value = summary_model$coefficients[, "Pr(>|z|)"],
    season = year
  )
  
  # Filter for relevant variables
  significant_coeffs <- coefficients %>%
    filter(
      grepl("point_diff", variable)
    )
  
  deviance = data.frame(
    model_deviance = summary(bt_model)$deviance,
    null_deviance = summary(bt_model)$null.deviance,
    season = year)
  
  
  # Return both ranking and coefficients
  return(list(
    ranking = ranking,
    coefficients = significant_coeffs,
    deviance = deviance
  ))
}

# Process data for first season (2000) and initialize results
initial_output = process_model(ncaa_data, ncaa_teams, seeds, 2000)
combined_results = initial_output[[1]]
coefficient_results = initial_output[[2]]
deviance_results = initial_output[[3]]

# Loop through seasons 2001 to 2016 and append results
for(season in 2001:2016){
  output = process_model(ncaa_data, ncaa_teams, seeds, season)
  combined_results = rbind(combined_results, output[[1]])
  coefficient_results = rbind(coefficient_results, output[[2]])
  deviance_results = rbind(deviance_results, output[[3]])
}

# Compare actual and predicted seeds
comparison = combined_results %>% 
  na.omit() %>% 
  group_by(season) %>% 
  summarise(
    num = n(),                                          # Total teams in comparison
    na = 64 - num,                                      # Number of missing teams
    same = sum(bt_seed == seed),                        # Number of correctly matched seeds
    diff = sum((bt_seed - as.integer(seed)), na.rm = T) # Total difference between predicted and actual seeds
  )

coefficient_results = coefficient_results %>% select(estimate, std_error, z_value, p_value, season) 
rownames(coefficient_results) = NULL

# Summarize comparison results
comparison %>% summarise(mean(na), mean(same), mean(diff))

# Save results to CSV files
write_csv(combined_results, "ranking_comparison.csv")
write_csv(comparison, "differences.csv")
write_csv(coefficient_results, "points_diff_p_values.csv")
write_csv(deviance_results, "deviances.csv")






