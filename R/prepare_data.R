##### Load libraries -----------------------------------------------------------

library(fitzRoy)
library(dplyr)
library(stringr)
library(here)

##### Retrieve 'high-level' match data -----------------------------------------

# Retrieve AFLW match data
aflw_match_data <- get_aflw_match_data()

# Remove rows for matches that have not yet been played
aflw_match_data <- aflw_match_data %>%
  filter(!str_detect(Match.Id, "2019"))

##### Retrieve detailed match data ---------------------------------------------

# Vector of match IDs to retrieve detailed game statistics for
# Excluding match IDs for the 2019 season - yet to be played
match_id_vec <- aflw_match_data$Match.Id

# Retrieve detailed statistics for match IDs specified in match_id_vec
aflw_detailed <- get_aflw_detailed_data(match_id_vec)

##### Wrangle match data -------------------------------------------------------

# Create a subset of aflw_match_data for HOME teams
home_teams <- aflw_match_data %>%
  select(Match.Id, Round.Id, Competition.Id, Venue, Local.Start.Time,
         Round.Number, Round.Abbreviation, Weather.Type, Weather.Description,
         Temperature, starts_with("Home"), Away.Points) %>%
  # Rename variables for ease of merging and analysis
  rename(team = Home.Team, goals = Home.Goals, behinds = Home.Behinds,
         points_for = Home.Points, points_against = Away.Points,
         behinds_left = Home.Left.Behinds, behinds_right = Home.Right.Behinds,
         posters_left = Home.Left.Posters, posters_right = Home.Right.Posters,
         behinds_rushed = Home.Rushed.Behinds,
         behinds_touched = Home.Touched.Behinds) %>%
  mutate(home_or_away = "Home")

# Create a subset of aflw_match_data for AWAY teams
away_teams <- aflw_match_data %>%
  select(Match.Id, Round.Id, Competition.Id, Venue, Local.Start.Time,
         Round.Number, Round.Abbreviation, Weather.Type, Weather.Description,
         Temperature, starts_with("Away"), Home.Points) %>%
  # Rename variables for ease of merging and analysis
  rename(team = Away.Team, goals = Away.Goals, behinds = Away.Behinds,
         points_for = Away.Points, points_against = Home.Points,
         behinds_left = Away.Left.Behinds, behinds_right = Away.Right.Behinds,
         posters_left = Away.Left.Posters, posters_right = Away.Right.Posters,
         behinds_rushed = Away.Rushed.Behinds,
         behinds_touched = Away.Touched.Behinds) %>%
  mutate(home_or_away = "Away")

# Row-bind the home and away subsets to create new data structure
# where one row == one team in each match, so each match will have two rows
aflw_match_data_clean <- rbind(home_teams, away_teams)

# Re-order the data set
aflw_match_data_clean <- aflw_match_data_clean[order(
  aflw_match_data_clean$Match.Id,
  rev(aflw_match_data_clean$home_or_away),
  decreasing = FALSE), ]

# Create new variables for match outcomes
aflw_match_data_clean <- aflw_match_data_clean %>%
  mutate(score_margin = points_for - points_against, # continuous variable
         match_outcome = case_when(                  # categorical variable
           score_margin > 0 ~ "Win",
           score_margin < 0 ~ "Loss",
           TRUE             ~ "Draw"))

##### Wrangle detailed data ----------------------------------------------------

# Subset to variables that will be useful for the analysis
# Variables are excluded here because they don't contain useful or correct data
aflw_detailed_selected <- aflw_detailed %>%
  select(Match.Id, contains("stats.totals"), contains("away.team"),
         contains("home.team"), -contains("behinds"), -contains("goals"),
         -contains("interChange"), -contains("lastUpdated"),
         -contains("metresGained"), -contains("ranking"),
         -contains("ratingPoints"), -contains("scoreInvolvements"),
         -contains("superGoals"))

# Create a subset of aflw_detailed_selected for HOME teams
aflw_detailed_home <- aflw_detailed_selected %>%
  select(Match.Id, contains("home.team"),
         contains("stats.totals"), -contains("away")) %>%
  # Rename variables for ease of merging and analysis
  rename(team_abbr = home.team.teamAbbr, team_id = home.team.teamId,
         team = home.team.teamName, team_nickname = home.team.teamNickname,
         bounces = home.stats.totals.bounces,
         clangers = home.stats.totals.clangers,
         clearances_centre = home.stats.totals.clearances.centreClearances,
         clearances_stoppage = home.stats.totals.clearances.stoppageClearances,
         clearances_total = home.stats.totals.clearances.totalClearances,
         marks_contested = home.stats.totals.contestedMarks,
         possessions_contested = home.stats.totals.contestedPossessions,
         disposals_efficiency = home.stats.totals.disposalEfficiency,
         disposals = home.stats.totals.disposals,
         dream_team_points = home.stats.totals.dreamTeamPoints,
         frees_against = home.stats.totals.freesAgainst,
         frees_for = home.stats.totals.freesFor,
         goal_accuracy = home.stats.totals.goalAccuracy,
         goal_assists = home.stats.totals.goalAssists,
         goal_efficiency = home.stats.totals.goalEfficiency,
         handballs = home.stats.totals.handballs,
         hitouts = home.stats.totals.hitouts,
         inside50s = home.stats.totals.inside50s,
         intercepts = home.stats.totals.intercepts,
         kicks = home.stats.totals.kicks, marks = home.stats.totals.marks,
         marks_inside50 = home.stats.totals.marksInside50,
         one_percenters = home.stats.totals.onePercenters,
         rebound50s = home.stats.totals.rebound50s,
         shot_efficiency = home.stats.totals.shotEfficiency,
         shots_at_goal = home.stats.totals.shotsAtGoal,
         tackles = home.stats.totals.tackles,
         tackles_inside50 = home.stats.totals.tacklesInside50,
         possessions_total = home.stats.totals.totalPossessions,
         turnovers = home.stats.totals.turnovers,
         possessions_uncontested = home.stats.totals.uncontestedPossessions)

# Create a subset of aflw_detailed_selected for AWAY teams
aflw_detailed_away <- aflw_detailed_selected %>%
  select(Match.Id, contains("away.team"),
         contains("stats.totals"), -contains("home")) %>%
  # Rename variables for ease of merging and analysis
  rename(team_abbr = away.team.teamAbbr, team_id = away.team.teamId,
         team = away.team.teamName, team_nickname = away.team.teamNickname,
         bounces = away.stats.totals.bounces,
         clangers = away.stats.totals.clangers,
         clearances_centre = away.stats.totals.clearances.centreClearances,
         clearances_stoppage = away.stats.totals.clearances.stoppageClearances,
         clearances_total = away.stats.totals.clearances.totalClearances,
         marks_contested = away.stats.totals.contestedMarks,
         possessions_contested = away.stats.totals.contestedPossessions,
         disposals_efficiency = away.stats.totals.disposalEfficiency,
         disposals = away.stats.totals.disposals,
         dream_team_points = away.stats.totals.dreamTeamPoints,
         frees_against = away.stats.totals.freesAgainst,
         frees_for = away.stats.totals.freesFor,
         goal_accuracy = away.stats.totals.goalAccuracy,
         goal_assists = away.stats.totals.goalAssists,
         goal_efficiency = away.stats.totals.goalEfficiency,
         handballs = away.stats.totals.handballs,
         hitouts = away.stats.totals.hitouts,
         inside50s = away.stats.totals.inside50s,
         intercepts = away.stats.totals.intercepts,
         kicks = away.stats.totals.kicks, marks = away.stats.totals.marks,
         marks_inside50 = away.stats.totals.marksInside50,
         one_percenters = away.stats.totals.onePercenters,
         rebound50s = away.stats.totals.rebound50s,
         shot_efficiency = away.stats.totals.shotEfficiency,
         shots_at_goal = away.stats.totals.shotsAtGoal,
         tackles = away.stats.totals.tackles,
         tackles_inside50 = away.stats.totals.tacklesInside50,
         possessions_total = away.stats.totals.totalPossessions,
         turnovers = away.stats.totals.turnovers,
         possessions_uncontested = away.stats.totals.uncontestedPossessions)

# Row-bind the home and away subsets to create new data structure
# where one row == one team in each match, so each match will have two rows
aflw_detailed_clean <- rbind(aflw_detailed_home, aflw_detailed_away)

##### Merge match and detailed data sets ---------------------------------------

aflw_merged <- left_join(aflw_match_data_clean, aflw_detailed_clean)

##### Export to CSV ------------------------------------------------------------

write.csv(aflw_merged, here("output/aflw_merged.csv"), row.names = FALSE)