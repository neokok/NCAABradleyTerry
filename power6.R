library(dplyr)

ncaa_teams = read_csv("ncaa_teams.csv")
ncaa_teams$Team = ncaa_teams$school_ncaa
results = read_csv("ranking_comparison.csv")
merged_data <- merge(ncaa_teams, results, by = "Team")

target_conferences<- c("PAC12", "BIG10", "BIGEAST", "BIG12", "ACC", "SEC")

team_counts <- merged_data %>%
  group_by(season, conf_alias) %>%
  summarise(count = n(), .groups = "drop")

total_counts <- team_counts %>%
  group_by(season) %>%
  summarise(total_teams = sum(count), .groups = "drop")

proportions <- team_counts %>%
  left_join(total_counts, by = "season") %>%
  mutate(proportion = ifelse(conf_alias %in% target_conferences, count / total_teams, 0))

print(proportions)

average_proportions <- proportions %>%
  group_by(conf_alias) %>%
  summarise(avg_proportion = mean(proportion, na.rm = TRUE)) %>%
  arrange(desc(avg_proportion))


print(average_proportions)

proportions%>%group_by(season)%>%summarize(x = n())

proportions%>%group_by(season)%>%summarize(x = n())%>%ungroup()%>%summarize(x = mean(x))

