!pip install pulp

from google.colab import files
from google.colab import drive
import pandas as pd
from datetime import datetime, timedelta

# https://www.dimers.com/bet-hub/nfl/schedule
# Mount Goole Drive and update filepath to match your relevant file location.
## You will need access to the two accompanying CSVs: "2023_full_set_cleaned.csv" and "co_geography.csv".
drive.mount('/content/drive')
file_path = '/content/drive/MyDrive/sports/NFL/Survivor/nfl-odds-rotowire.csv'
selected_teams_file_path  = '/content/drive/MyDrive/sports/NFL/Survivor/selected_teams.csv'

import pandas as pd
from datetime import timedelta
from pulp import LpMinimize, LpProblem, LpVariable, lpSum

# Read the CSV file
data = pd.read_csv(file_path)

# Read the CSV file for already selected teams
selected_teams_df = pd.read_csv(selected_teams_file_path)

# Convert the Date column to datetime format
data['Date'] = pd.to_datetime(data['Date'])

# Function to calculate the custom week number
def get_custom_week(date):
    # Adjust the date to start the week on Tuesday
    start_of_week = date - timedelta(days=(date.weekday() - 1) % 7)
    return start_of_week.strftime('%Y-%m-%d')

# Apply the function to calculate the custom week
data['Week'] = data['Date'].apply(get_custom_week)

# Extract unique weeks and teams
weeks = sorted(data['Week'].unique())
teams = sorted(data['Team'].unique())

# Extract the list of already selected teams
already_selected_teams = selected_teams_df['Team'].unique()

# Filter out already selected teams from consideration
teams = [team for team in teams if team not in already_selected_teams]

# Verify the results
print(data.head())
print(f"Teams available for selection: {teams}")

# Initialize the problem
prob = LpProblem("NFL_Survivor_Pool", LpMinimize)

# Create decision variables only for available teams
decision_vars = {(team, week): LpVariable(f"x_{team}_{week}", cat='Binary')
                 for team in teams for week in weeks}

# Objective function: maximize total spread
obj_expr = lpSum(
    data.loc[(data['Team'] == team) & (data['Week'] == week), 'Spread'].values[0] * decision_vars[(team, week)]
    for team in teams for week in weeks
    if not data[(data['Team'] == team) & (data['Week'] == week)].empty
)

prob += obj_expr, "Total_Spread"

# Constraints: Exactly one team per week
for week in weeks:
    prob += lpSum(
        decision_vars[(team, week)]
        for team in teams
        if not data[(data['Team'] == team) & (data['Week'] == week)].empty
    ) == 1, f"One_team_per_week_{week}"

# Constraint: A team cannot be selected more than once during the season
for team in teams:
    prob += lpSum(decision_vars[(team, week)] for week in weeks) <= 1, f"Team_{team}_selected_once"

# Update Opponent Calculation
# Assume 'Game' column indicates which game the team is playing
data['Opponent'] = data.apply(lambda row:
    data[(data['Week'] == row['Week']) & (data['Game'] == row['Game']) & (data['Team'] != row['Team'])]['Team'].values[0]
    if len(data[(data['Week'] == row['Week']) & (data['Game'] == row['Game']) & (data['Team'] != row['Team'])]) > 0 else None, axis=1)

# Constraint: Cannot select against the same opponent three times in a row
for i in range(len(weeks) - 2):  # Check windows of 3 weeks
    week1 = weeks[i]
    week2 = weeks[i + 1]
    week3 = weeks[i + 2]

    for team in teams:
        # Opponents for this team over the three-week window
        opponents_week1 = data[(data['Week'] == week1) & (data['Team'] == team)]['Opponent'].values
        opponents_week2 = data[(data['Week'] == week2) & (data['Team'] == team)]['Opponent'].values
        opponents_week3 = data[(data['Week'] == week3) & (data['Team'] == team)]['Opponent'].values

        # Get the common opponent in three weeks
        common_opponent = set(opponents_week1).intersection(opponents_week2).intersection(opponents_week3)

        if common_opponent:
            opponent = list(common_opponent)[0]  # Taking the first common opponent
            # Constraint to prevent playing against the same opponent three times in a row
            prob += lpSum(
                decision_vars[(team, week1)] + decision_vars[(team, week2)] + decision_vars[(team, week3)]
                for team in teams if team in [data[(data['Week'] == week1) & (data['Opponent'] == opponent)]['Team'].values,
                                              data[(data['Week'] == week2) & (data['Opponent'] == opponent)]['Team'].values,
                                              data[(data['Week'] == week3) & (data['Opponent'] == opponent)]['Team'].values]
            ) <= 2, f"Three_in_a_row_{team}_{opponent}_{week1}_{week2}_{week3}"

# Solve the problem
prob.solve()

# Print the results
selected_teams = [(var.name, var.value()) for var in prob.variables() if var.value() == 1]
print("Selected teams and weeks:")
for var_name, value in selected_teams:
    print(f"{var_name} = {value}")

print(f"Total Spread: {prob.objective.value()}")
